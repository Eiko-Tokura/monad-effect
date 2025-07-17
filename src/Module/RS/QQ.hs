-- | This module provides Template Haskell utilities for generating RModules and SModules with fixed type
--
-- The `makeRModule` function generates a reader module, for example
--
-- given the following information:
--
-- [makeRModule|MyModule
--   myRecord1 :: !MyType1
--   myRecord2 :: MyType2
-- |]
--
-- it should generate
--
-- data MyModule
--
-- instance Module MyModule where
--   data ModuleRead MyModule  = MyModuleRead { myRecord1 :: !MyType1, myRecord2 :: MyType2 }
--   data ModuleState MyModule = MyModuleState deriving (Generic, NFData)
--
-- runMyModule :: MyModuleRead -> EffT' (MyModule : mods) errs m a -> EffT' mods errs m a
-- runMyModule r = runEffT_ r MyModuleState
-- {-# INLINE runMyModule #-}
--
-- runRModuleIn :: (ConsFDataList c mods, RemoveElem c mods, Monad m, In' c MyModule mods) => MyModuleRead -> EffT' c mods es m a -> EffT' c (Remove (FirstIndex MyModule mods) mods) es m a
-- runRModuleIn r = runEffTIn_ r MyModuleState
-- {-# INLINE runMyModuleIn #-}
module Module.RS.QQ where

import Control.Monad
import Control.Monad.Effect
import Data.TypeList
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.Parsec
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

data RModuleSpec = RModuleSpec
  { rModuleName   :: Name
  , rModuleFields :: [(Name, Bool, Type)] -- ^ (fieldName, strictness, fieldType)
  }
  deriving Show

makeRModule :: QuasiQuoter
makeRModule = QuasiQuoter
  { quoteExp  = error "makeRModule: should be used as top-level declaration only, not as an expression"
  , quotePat  = error "makeRModule: should be used as top-level declaration only, not as a pattern"
  , quoteType = error "makeRModule: should be used as top-level declaration only, not as a type"
  , quoteDec  = parseRModule
  }

parseRModule :: String -> Q [Dec]
parseRModule input = do
  let spec = runIdentity $ runParserT parserRModuleSpec () "parseRModule" input
  case spec of
    Left err -> fail $ "Failed to parse RModule: " ++ show err
    Right rModuleSpecs -> concat <$> mapM generateRModule rModuleSpecs

parserRModuleSpec :: ParsecT String () Identity [RModuleSpec]
parserRModuleSpec = many $ do
  spaces
  name   <- parseModuleName
  void $ manyTill space endOfLine
  fields <- many (many1 space >> parseField)
  return $ RModuleSpec name fields

parseModuleName :: ParsecT String () Identity Name
parseModuleName = do
  upperHead <- upper
  rest      <- many (alphaNum <|> char '_')
  let moduleName = upperHead : rest
  pure $ mkName moduleName

parseField :: ParsecT String () Identity (Name, Bool, Type)
parseField = do
  fieldName <- parseFieldName
  spaces >> char ':' >> char ':' >> spaces
  strictness <- option False (True <$ char '!')
  typeName <- mkName <$> do
    typeHead <- upper
    typeRest <- manyTill anyChar (try (eof <|> void endOfLine))
    pure $ typeHead : typeRest
  return (fieldName, strictness, ConT typeName)
  where
    parseFieldName = mkName <$> do
      lowerHead <- lower
      rest      <- many (alphaNum <|> char '_' <|> char '\'')
      pure $ lowerHead : rest

-------------------------------------------------------------------------------
-- Code generation
-------------------------------------------------------------------------------

generateRModule :: RModuleSpec -> Q [Dec]
generateRModule RModuleSpec{rModuleName = modName, rModuleFields = flds} = do
  ---------------------------------------------------------------------------
  -- Helper names
  ---------------------------------------------------------------------------
  let readConName  = mkName $ nameBase modName ++ "Read"
      readTy       = ConT ''ModuleRead `AppT` ConT modName
      stateConName = mkName $ nameBase modName ++ "State"
      runName      = mkName $ "run" ++ nameBase modName
      runInName    = mkName $ "run" ++ nameBase modName ++ "In"

      mkBang True  = Bang NoSourceUnpackedness SourceStrict
      mkBang False = Bang NoSourceUnpackedness NoSourceStrictness

  ---------------------------------------------------------------------------
  -- 1. data <ModuleTag>
  ---------------------------------------------------------------------------
  let tagDec = DataD [] modName [] Nothing [] []

  ---------------------------------------------------------------------------
  -- 2. the two associated data instances inside  `instance Module ...`
  ---------------------------------------------------------------------------
  recFields <- forM flds $ \(n, bng, ty) ->
                 pure (n, mkBang bng, ty)

  let readData  = DataInstD
                    []                      -- context
                    Nothing                 -- maybe kind
                    (AppT (ConT ''ModuleRead) (ConT modName))
                    Nothing
                    [RecC readConName recFields]
                    derivs

      stateData = DataInstD
                    [] Nothing
                    (AppT (ConT ''ModuleState) (ConT modName))
                    Nothing
                    [NormalC stateConName []]
                    derivs

      derivs = [ DerivClause (Just StockStrategy)   [ConT ''Generic]
               , DerivClause (Just AnyclassStrategy) [ConT ''NFData]
               ]

      inst = InstanceD Nothing [] (AppT (ConT ''Module) (ConT modName))
               [readData, stateData]

  ---------------------------------------------------------------------------
  -- 3. run<MyModule>
  ---------------------------------------------------------------------------
  modsTv <- newName "mods"
  errsTv <- newName "errs"
  mTv    <- newName "m"
  aTv    <- newName "a"

  let modsV = VarT modsTv
      errsV = VarT errsTv
      mV    = VarT mTv
      aV    = VarT aTv

      consMods = PromotedConsT `AppT` ConT modName `AppT` modsV
      eff4 = foldl AppT (ConT ''EffT)

      runSigType =
        ForallT [ PlainTV modsTv SpecifiedSpec, PlainTV errsTv SpecifiedSpec
                , PlainTV mTv SpecifiedSpec,    PlainTV aTv SpecifiedSpec ]
                [ AppT (ConT ''Monad) mV
                , ConT ''ConsFDataList `AppT` ConT ''FData `AppT` (ConT '(:) `AppT` ConT modName `AppT` modsV)
                ]
                (readTy `arr`
                   eff4 [consMods, errsV, mV, aV] `arr`
                   eff4 [modsV    , errsV, mV, aV])

  runSig <- sigD runName (pure runSigType)
  runFun <- funD runName
              [ clause [varP (mkName "r")]
                       (normalB (varE 'runEffTOuter_ `appE`
                                 varE (mkName "r")   `appE`
                                 conE stateConName))
                       []
              ]
  runPrag <- pragInlD runName Inline FunLike AllPhases

  ---------------------------------------------------------------------------
  -- 4. run<MyModule>In
  ---------------------------------------------------------------------------
  cTv  <- newName "c"
  esTv <- newName "es"

  let cV    = VarT cTv
      esV   = VarT esTv

      eff5 = foldl AppT (ConT ''EffT')

      removeMods =
        AppT (AppT (ConT ''Remove)
                   (AppT (AppT (ConT ''FirstIndex) (ConT modName)) modsV))
             modsV

      ctx = [ AppT (AppT (ConT ''ConsFDataList) cV) modsV
            , AppT (AppT (ConT ''RemoveElem)    cV) modsV
            , AppT (ConT ''Monad)               mV
            , AppT (AppT (AppT (ConT ''In') cV) (ConT modName)) modsV
            ]

      runInSigType =
        ForallT [ PlainTV cTv SpecifiedSpec, PlainTV modsTv SpecifiedSpec, PlainTV esTv SpecifiedSpec
                , PlainTV mTv SpecifiedSpec, PlainTV aTv SpecifiedSpec ]
                ctx
                (readTy `arr`
                   eff5 [cV, modsV, esV, mV, aV] `arr`
                   eff5 [cV, removeMods, esV, mV, aV])

  runInSig <- sigD runInName (pure runInSigType)
  runInFun <- funD runInName
                [ clause [varP (mkName "r")]
                         (normalB (varE 'runEffTIn_ `appE`
                                   varE (mkName "r") `appE`
                                   conE stateConName))
                         []
                ]
  runInPrag <- pragInlD runInName Inline FunLike AllPhases

  ---------------------------------------------------------------------------
  -- 5. spit everything out
  ---------------------------------------------------------------------------
  pure [ tagDec
       , inst
       , runSig , runFun , runPrag
       , runInSig , runInFun , runInPrag
       ]

-------------------------------------------------------------------------------
-- Helpers (TH arrow type)
-------------------------------------------------------------------------------
arr :: Type -> Type -> Type
arr = AppT . AppT ArrowT
infixr 5 `arr`
