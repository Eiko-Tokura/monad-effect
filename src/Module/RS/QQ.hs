{-# LANGUAGE RecordWildCards, OverloadedRecordDot, ViewPatterns #-}
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
-- @
-- data MyModule
--
-- type MyModuleRead = ModuleRead MyModule
--
-- instance Module MyModule where
--   data ModuleRead MyModule  = MyModuleRead { myRecord1 :: !MyType1, myRecord2 :: MyType2 }
--   data ModuleState MyModule = MyModuleState deriving (Generic, NFData)
--
-- runMyModule :: (ConsFDataList c mods, Monad m) => ModuleRead MyModule -> EffT' c (MyModule : mods) errs m a -> EffT' mods errs m a
-- runMyModule r = runEffTOuter_ r MyModuleState
-- {-# INLINE runMyModule #-}
--
-- runRModuleIn :: (ConsFDataList c mods, RemoveElem c mods, Monad m, In' c MyModule mods) => ModuleRead MyModule -> EffT' c mods es m a -> EffT' c (Remove (FirstIndex MyModule mods) mods) es m a
-- runRModuleIn r = runEffTIn_ r MyModuleState
-- {-# INLINE runMyModuleIn #-}
-- @
--
--
-- @
-- [makeRSModule|
-- MyRSModule
--   Read  myField1 :: !MyType1
--   Read  myField2 :: MyType2
--   State myStateField1 :: !MyStateType1
--   State myStateField2 :: MyStateType2
-- |]
-- @
--
-- it should generate
--
-- * data MyRSModule
-- * generate data instances for Module <MyModule>
-- * generate run<MyModule>, run<MyModule>', run<MyModule>_ and run<MyModule>In, run<MyModule>In', run<MyModule>In_ functions
-- * generate type synonym for ModuleRead <MyModule> and ModuleState <MyModule>
module Module.RS.QQ
  ( makeRModule,  makeRModule_, makeRModule__
  , makeRSModule, makeRSModule_
  ) where

import Control.DeepSeq (NFData)
import Control.Monad
import Control.Monad.Effect
import Data.Default
import Data.Either
import Data.TypeList
import GHC.Generics (Generic)
import Language.Haskell.Meta.Parse
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.Parsec

data DataFieldSpec = DataFieldSpec
  { fieldName   :: Name
  , fieldLens   :: Bool -- ^ Whether to generate a lens for this field
  , fieldStrict :: Bool
  , fieldType   :: Type
  }
  deriving Show

data DataConsSpec = DataConsSpec
  { dataConsName :: Name
  , dataFields   :: [DataFieldSpec]
  }
  deriving Show

data RSModuleSpec = RSModuleSpec
  { typeName  :: Name
  , readSpec  :: DataConsSpec
  , stateSpec :: DataConsSpec
  }
  deriving Show

data GenerationConfig = GenerationConfig
  { deriveConfigs          :: [DeriveConfig]
  , generateSystemInstance :: Bool -- ^ Only for RModule
  }

instance Default GenerationConfig where
  def = GenerationConfig
    { deriveConfigs          = [ConfigDeriveGeneric, ConfigDeriveNFData]
    , generateSystemInstance = True
    }

-- | Generates all the boilerplate declarations for a reader-like module.
--
-- Includes:
--
-- * data MyModule
--
-- * instance Module MyModule
--
-- * instance SystemModule MyModule
--
-- * runMyModule
--
-- * runMyModuleIn
--
-- * A type synonym `type MyModuleRead = ModuleRead MyModule`
--
makeRModuleConf :: GenerationConfig -> QuasiQuoter
makeRModuleConf conf = QuasiQuoter
  { quoteExp  = error "makeRModule: should be used as top-level declaration only, not as an expression"
  , quotePat  = error "makeRModule: should be used as top-level declaration only, not as a pattern"
  , quoteType = error "makeRModule: should be used as top-level declaration only, not as a type"
  , quoteDec  = parseRModule conf
  }

makeRSModuleConf :: GenerationConfig -> QuasiQuoter
makeRSModuleConf conf = QuasiQuoter
  { quoteExp  = error "makeRSModule: should be used as top-level declaration only, not as an expression"
  , quotePat  = error "makeRSModule: should be used as top-level declaration only, not as a pattern"
  , quoteType = error "makeRSModule: should be used as top-level declaration only, not as a type"
  , quoteDec  = parseRSModule conf
  }

makeRModule, makeRModule_, makeRModule__, makeRSModule, makeRSModule_ :: QuasiQuoter
-- | Make RModule with Generic and NFData derivations, and SystemModule instance
makeRModule   = makeRModuleConf $ GenerationConfig [ConfigDeriveGeneric, ConfigDeriveNFData] True
-- | Make RModule without any derivations, but with SystemModule instance
makeRModule_  = makeRModuleConf $ GenerationConfig [] True
-- | Make RModule without any derivations and without SystemModule instance
makeRModule__  = makeRModuleConf $ GenerationConfig [] False
-- | Make RSModule with Generic and NFData derivations
makeRSModule  = makeRSModuleConf $ GenerationConfig [ConfigDeriveGeneric, ConfigDeriveNFData] False
-- | Make RSModule without any derivations
makeRSModule_ = makeRSModuleConf $ GenerationConfig [] False

data DeriveConfig = ConfigDeriveGeneric | ConfigDeriveNFData
  deriving (Show, Eq)

parseRModule :: GenerationConfig -> String -> Q [Dec]
parseRModule conf input = do
  let spec = runIdentity $ runParserT (many parseDataConsSpec) () "parseRModule" input
  case spec of
    Left err -> fail $ "Failed to parse RModule: " ++ show err
    Right rModuleSpecs -> concat <$> mapM (generateRModule conf) rModuleSpecs

parseRSModule :: GenerationConfig -> String -> Q [Dec]
parseRSModule conf input = do
  let spec = runIdentity $ runParserT parseRSModuleSpec () "parseRSModule" input
  case spec of
    Left err -> fail $ "Failed to parse RSModule: " ++ show err
    Right rsModuleSpec -> generateRSModule conf rsModuleSpec

parseDataConsSpec :: ParsecT String () Identity DataConsSpec
parseDataConsSpec = do
  spaces
  name   <- parseModuleName
  void $ manyTill space endOfLine
  fields <- many (many1 space >> parseField)
  return $ DataConsSpec name fields

-- @
-- MyRSModule
--   Read  myField1 :: !MyType1
--   Read  myField2 :: MyType2
--   State myStateField1 :: !MyStateType1
--   State myStateField2 :: MyStateType2
-- @
parseRSModuleSpec :: ParsecT String () Identity RSModuleSpec
parseRSModuleSpec = do
  spaces
  modName <- parseModuleName
  let readName  = mkName $ nameBase modName ++ "Read"
      stateName = mkName $ nameBase modName ++ "State"
  void $ manyTill space endOfLine
  eitherFields <- many (try (many1 space >> optional (string "Read"  >> many1 space) >> Left  <$> parseField)
                        <|> (many1 space >>           string "State" >> many1 space  >> Right <$> parseField)
                       )
  return $ RSModuleSpec
    { typeName  = modName
    , readSpec  = DataConsSpec readName  $ lefts eitherFields
    , stateSpec = DataConsSpec stateName $ rights eitherFields
    }

parseModuleName :: ParsecT String () Identity Name
parseModuleName = do
  upperHead <- upper
  rest      <- many (alphaNum <|> char '_')
  let moduleName = upperHead : rest
  pure $ mkName moduleName

parseField :: ParsecT String () Identity DataFieldSpec
parseField = do
  lens <- option False (True <$ try (string "Lens" >> many1 space))
  fieldName <- parseFieldName
  spaces >> char ':' >> char ':' >> spaces
  strictness <- option False (True <$ char '!')
  typeString <- manyTill anyChar (try (eof <|> void endOfLine))
  case parseType typeString of
    Left err -> fail $ "Failed to parse type: " ++ show err
    Right type' -> return $ DataFieldSpec
      { fieldName   = fieldName
      , fieldLens   = lens
      , fieldStrict = strictness
      , fieldType   = type'
      }
    -- (fieldName, strictness, type')
  where
    parseFieldName = mkName <$> do
      lowerHead <- lower
      rest      <- many (alphaNum <|> char '_' <|> char '\'')
      pure $ lowerHead : rest

-------------------------------------------------------------------------------
-- Code generation
-------------------------------------------------------------------------------
deriveG :: DerivClause
deriveG = DerivClause (Just StockStrategy)   [ConT ''Generic]

deriveNF :: DerivClause
deriveNF = DerivClause (Just AnyclassStrategy) [ConT ''NFData]

mkBang :: Bool -> Bang
mkBang True  = Bang NoSourceUnpackedness SourceStrict
mkBang False = Bang NoSourceUnpackedness NoSourceStrictness

updateBang :: DataFieldSpec -> (Name, Bang, Type)
updateBang DataFieldSpec{..} = (fieldName, mkBang fieldStrict, fieldType)

appendName :: String -> DataFieldSpec -> DataFieldSpec
appendName suffix DataFieldSpec{..} =
  DataFieldSpec
    { fieldName   = mkName $ nameBase fieldName ++ suffix
    , fieldLens   = fieldLens
    , fieldStrict = fieldStrict
    , fieldType   = fieldType
    }

inlinePragma :: Name -> Dec
inlinePragma name = PragmaD $ InlineP name Inline FunLike AllPhases

data DataInstanceSpec = DataInstanceSpec
  { dataFamilyType        :: Type
  , dataFamilyInputType   :: Type
  , dataFamilyConstructor :: DataConsSpec
  , dataFamilyDerivations :: [DerivClause]
  } deriving Show

-- | Generate a data family instance declaration. 
-- Automatically switch to newtype if there is only one record field with strictness enabled.
dataInstance :: DataInstanceSpec -> Dec
dataInstance DataInstanceSpec{..} =
  case dataFields dataFamilyConstructor of
    [fieldStrict -> True] ->
      NewtypeInstD [] Nothing
        (AppT dataFamilyType dataFamilyInputType)
        Nothing
        (RecC (dataConsName dataFamilyConstructor) $ cancelBang <$> dataFields dataFamilyConstructor)
        dataFamilyDerivations
    _ ->
      DataInstD [] Nothing
        (AppT dataFamilyType dataFamilyInputType)
        Nothing
        [RecC (dataConsName dataFamilyConstructor) $ updateBang <$> dataFields dataFamilyConstructor]
        dataFamilyDerivations
  where cancelBang fspec = (fieldName fspec, mkBang False, fieldType fspec)

-- | If fieldName starts with '_', remove the leading underscore for the lens name.
-- Otherwise, prepend an underscore.
lensName :: Name -> Name
lensName (nameBase -> ('_':rest)) = mkName rest
lensName name                     = mkName $ '_' : nameBase name

generateLens :: Type -> DataFieldSpec -> Q [Dec]
generateLens recType DataFieldSpec{..} | fieldLens = do
  let lensFunName = lensName fieldName
      -- | The body of the lens function
      -- _lensName :: Functor f => (fieldType -> f fieldType) -> (DataConsType -> f DataConsType)
      -- _lensName f s = fmap (\x -> s { fieldName = x }) (f (fieldName s))
      lensTypeSig = ForallT [PlainTV funName SpecifiedSpec]
                            [ConT ''Functor `AppT` VarT funName]
                            (ArrowT `AppT` (ArrowT `AppT` fieldType `AppT` (VarT funName `AppT` fieldType))
                                    `AppT` (ArrowT `AppT` recType   `AppT` (VarT funName `AppT` recType))
                            )
      lensFunBody = LamE [VarP fName, VarP sName]
                    $ VarE 'fmap
                      `AppE` LamE [VarP xName] (RecUpdE (VarE sName) [(fieldName, VarE xName)])
                      `AppE` AppE (VarE fName) (AppE (VarE fieldName) (VarE sName))
      funName = mkName "fun"
      fName   = mkName "f"
      sName   = mkName "s"
      xName   = mkName "x"
  sig <- sigD lensFunName (pure lensTypeSig)
  fun <- funD lensFunName [clause [] (normalB (pure lensFunBody)) []]
  prag <- pragInlD lensFunName Inline FunLike AllPhases
  return [sig, fun, prag]
generateLens _ DataFieldSpec{} = return []

-- * generate data instances for Module <MyModule>
-- * generate run<MyModule>, run<MyModule>', run<MyModule>_ and run<MyModule>In, run<MyModule>In', run<MyModule>In_ functions
-- * generate type synonym for ModuleRead <MyModule> and ModuleState <MyModule>
generateRSModule :: GenerationConfig -> RSModuleSpec -> Q [Dec]
generateRSModule GenerationConfig { deriveConfigs = dconf } RSModuleSpec{typeName, readSpec, stateSpec} = do
  let deriveGeneric = [deriveG  | ConfigDeriveGeneric `elem` dconf]
      -- deriveNFData  = [deriveNF | ConfigDeriveNFData  `elem` dconf]

  let warnStateNonStrict = any (\fspec -> not fspec.fieldStrict) (dataFields stateSpec)

  when warnStateNonStrict $ reportWarning
    $  "The state record for the module " <> nameBase typeName
    <> " has non-strict fields. This may lead to lazy thunk leaks in case of infinite updates without evaluation."

  let dataTag = DataD [] typeName [] Nothing [] []

      instanceModule = InstanceD Nothing [] (AppT (ConT ''Module) (ConT typeName))
        [ dataInstance DataInstanceSpec
            { dataFamilyType        = ConT ''ModuleRead
            , dataFamilyInputType   = ConT typeName
            , dataFamilyConstructor = readSpec
            , dataFamilyDerivations = deriveGeneric
            }
        , dataInstance DataInstanceSpec
            { dataFamilyType        = ConT ''ModuleState
            , dataFamilyInputType   = ConT typeName
            , dataFamilyConstructor = stateSpec
            , dataFamilyDerivations = deriveGeneric
            }
        ]
      readType     = ConT ''ModuleRead  `AppT` ConT typeName
      stateType    = ConT ''ModuleState `AppT` ConT typeName
      typeSynRead  = TySynD (dataConsName readSpec)  [] readType
      typeSynState = TySynD (dataConsName stateSpec) [] stateType

      runMyModuleName    = mkName $ "run" ++ nameBase typeName
      runMyModule'Name   = mkName $ "run" ++ nameBase typeName ++ "'"
      runMyModule_Name   = mkName $ "run" ++ nameBase typeName ++ "_"
      runMyModuleInName  = mkName $ "run" ++ nameBase typeName ++ "In"
      runMyModuleIn'Name = mkName $ "run" ++ nameBase typeName ++ "In'"
      runMyModuleIn_Name = mkName $ "run" ++ nameBase typeName ++ "In_"

{-
runEffTOuter :: forall mod mods es m c a. (ConsFDataList c (mod : mods), ConsFData1 c mods, Monad m)
  => ModuleRead mod -> ModuleState mod -> EffT' c (mod : mods) es m a -> EffT' c mods es m (a, ModuleState mod)
       
runEffTOuter' :: forall mod mods es m c a. (ConsFDataList c (mod : mods), ConsFData1 c mods, Monad m)
  => ModuleRead mod -> ModuleState mod -> EffT' c (mod : mods) es m a -> EffT' c mods NoError m (Result es a, ModuleState mod)

runEffTOuter_ :: forall mod mods es m c a. (ConsFDataList c (mod : mods), ConsFData1 c mods, Monad m)
  => ModuleRead mod -> ModuleState mod -> EffT' c (mod : mods) es m a -> EffT' c mods es m a

runEffTIn :: forall mod mods es m c a. (RemoveElem c mods, Monad m, In' c mod mods)
  => ModuleRead mod -> ModuleState mod -> EffT' c mods es m a
  -> EffT' c (Remove (FirstIndex mod mods) mods) es m (a, ModuleState mod)

runEffTIn' :: forall mod mods es m c a. (RemoveElem c mods, Monad m, In' c mod mods)
  => ModuleRead mod -> ModuleState mod -> EffT' c mods es m a
  -> EffT' c (Remove (FirstIndex mod mods) mods) NoError m (Result es a, ModuleState mod)

runEffTIn_ :: forall mod mods es m c a. (RemoveElem c mods, Monad m, In' c mod mods)
  => ModuleRead mod -> ModuleState mod -> EffT' c mods es m a
  -> EffT' c (Remove (FirstIndex mod mods) mods) es m a
-}

      modsN = mkName "mods"
      errsN = mkName "errs"
      mN    = mkName "m"
      cN    = mkName "c"
      aN    = mkName "a"
      modsT = VarT modsN
      errsT = VarT errsN
      mT    = VarT mN
      cT    = VarT cN
      aT    = VarT aN
      effTTy = ConT ''EffT
      effT'Ty = ConT ''EffT'
      noErrorT = ConT ''NoError
      myModuleAndMods = ConT '(:) `AppT` ConT typeName `AppT` modsT
      moduleReadMyModule  = ConT ''ModuleRead `AppT` ConT typeName
      moduleStateMyModule = ConT ''ModuleState `AppT` ConT typeName

      runMyModuleSig = SigD runMyModuleName $
        ForallT [ PlainTV modsN SpecifiedSpec, PlainTV errsN SpecifiedSpec
        , PlainTV mN    SpecifiedSpec, PlainTV aN SpecifiedSpec
        ]
        [ AppT (ConT ''Monad) mT
        , ConT ''ConsFDataList `AppT` ConT ''FData `AppT` (ConT '(:) `AppT` ConT typeName `AppT` modsT)
        ]
        ( moduleReadMyModule `arr` moduleStateMyModule `arr` (effTTy `AppT` myModuleAndMods `AppT` errsT `AppT` mT `AppT` aT) `arr`
           (effTTy `AppT` modsT `AppT` errsT `AppT` mT `AppT` (ConT ''(,) `AppT` aT `AppT` moduleStateMyModule))
        )

      runMyModule'Sig = SigD runMyModule'Name $
        ForallT [ PlainTV modsN SpecifiedSpec, PlainTV errsN SpecifiedSpec
        , PlainTV mN    SpecifiedSpec, PlainTV aN SpecifiedSpec
        ]
        [ AppT (ConT ''Monad) mT
        , ConT ''ConsFDataList `AppT` ConT ''FData `AppT` (ConT '(:) `AppT` ConT typeName `AppT` modsT)
        ]
        ( moduleReadMyModule `arr` moduleStateMyModule `arr` (effTTy `AppT` myModuleAndMods `AppT` errsT `AppT` mT `AppT` aT) `arr`
           (effTTy `AppT` modsT `AppT` noErrorT `AppT` mT `AppT` (ConT ''(,) `AppT` (ConT ''Result `AppT` errsT `AppT` aT) `AppT` moduleStateMyModule))
        )

      runMyModule_Sig = SigD runMyModule_Name $
        ForallT [ PlainTV modsN SpecifiedSpec, PlainTV errsN SpecifiedSpec
        , PlainTV mN    SpecifiedSpec, PlainTV aN SpecifiedSpec
        ]
        [ AppT (ConT ''Monad) mT
        , ConT ''ConsFDataList `AppT` ConT ''FData `AppT` (ConT '(:) `AppT` ConT typeName `AppT` modsT)
        ]
        ( moduleReadMyModule `arr` moduleStateMyModule `arr` (effTTy `AppT` myModuleAndMods `AppT` errsT `AppT` mT `AppT` aT) `arr`
           (effTTy `AppT` modsT `AppT` errsT `AppT` mT `AppT` aT)
        )

      runMyModuleInSig = SigD runMyModuleInName $
        ForallT [ PlainTV modsN SpecifiedSpec, PlainTV errsN SpecifiedSpec
        , PlainTV mN    SpecifiedSpec, PlainTV cN SpecifiedSpec, PlainTV aN SpecifiedSpec
        ]
        [ ConT ''RemoveElem `AppT`  cT `AppT` modsT
        , ConT ''Monad      `AppT`  mT
        , ConT ''In'        `AppT`  cT `AppT` ConT typeName `AppT` modsT
        ]
        ( moduleReadMyModule `arr` moduleStateMyModule `arr`
          (effT'Ty `AppT` cT `AppT` modsT `AppT` errsT `AppT` mT `AppT` aT) `arr`
          (effT'Ty `AppT` cT `AppT` (ConT ''Remove `AppT` (ConT ''FirstIndex `AppT` ConT typeName `AppT` modsT) `AppT` modsT) `AppT`
           errsT `AppT` mT `AppT` (ConT ''(,) `AppT` aT `AppT` moduleStateMyModule))
        )

      runMyModuleIn'Sig = SigD runMyModuleIn'Name $
        ForallT [ PlainTV modsN SpecifiedSpec, PlainTV errsN SpecifiedSpec
        , PlainTV mN    SpecifiedSpec, PlainTV cN SpecifiedSpec, PlainTV aN SpecifiedSpec
        ]
        [ ConT ''RemoveElem  `AppT` cT `AppT` modsT
        , ConT ''Monad       `AppT` mT
        , ConT ''In'         `AppT` cT `AppT` ConT typeName `AppT` modsT
        ]
        ( moduleReadMyModule `arr` moduleStateMyModule `arr`
          (effT'Ty `AppT` cT `AppT` modsT `AppT` errsT `AppT` mT `AppT` aT) `arr`
          (effT'Ty `AppT` cT `AppT` (ConT ''Remove `AppT` (ConT ''FirstIndex `AppT` ConT typeName `AppT` modsT) `AppT` modsT) `AppT`
           noErrorT `AppT` mT `AppT` (ConT ''(,) `AppT` (ConT ''Result `AppT` errsT `AppT` aT) `AppT` moduleStateMyModule))
        )

      runMyModuleIn_Sig = SigD runMyModuleIn_Name $
        ForallT [ PlainTV modsN SpecifiedSpec, PlainTV errsN SpecifiedSpec
        , PlainTV mN    SpecifiedSpec, PlainTV cN SpecifiedSpec, PlainTV aN SpecifiedSpec
        ]
        [ ConT ''RemoveElem  `AppT` cT `AppT` modsT
        , ConT ''Monad       `AppT` mT
        , ConT ''In'         `AppT` cT `AppT` ConT typeName `AppT` modsT
        ]
        ( moduleReadMyModule `arr` moduleStateMyModule `arr`
          (effT'Ty `AppT` cT `AppT` modsT `AppT` errsT `AppT` mT `AppT` aT) `arr`
          (effT'Ty `AppT` cT `AppT` (ConT ''Remove `AppT` (ConT ''FirstIndex `AppT` ConT typeName `AppT` modsT) `AppT` modsT) `AppT` errsT `AppT` mT `AppT` aT)
        )

      runMyModuleFun = FunD runMyModuleName
        [ Clause [VarP (mkName "r"), VarP (mkName "s")]
                 (NormalB (VarE 'runEffTOuter `AppE`
                         VarE (mkName "r") `AppE`
                         VarE (mkName "s")))
                 []
        ]
      runMyModule'Fun = FunD runMyModule'Name
        [ Clause [VarP (mkName "r"), VarP (mkName "s")]
                 (NormalB $ VarE 'runEffTOuter' `AppE`
                         VarE (mkName "r") `AppE`
                         VarE (mkName "s")
                 )
                 []
        ]
      runMyModule_Fun = FunD runMyModule_Name
        [ Clause [VarP (mkName "r"), VarP (mkName "s")]
                 (NormalB $ VarE 'runEffTOuter_ `AppE`
                         VarE (mkName "r") `AppE`
                         VarE (mkName "s")
                 )
                 []
        ]
      runMyModuleInFun = FunD runMyModuleInName
        [ Clause [VarP (mkName "r"), VarP (mkName "s")]
                 (NormalB $ VarE 'runEffTIn `AppE`
                         VarE (mkName "r") `AppE`
                         VarE (mkName "s")
                 )
                 []
        ]
      runMyModuleIn'Fun = FunD runMyModuleIn'Name
        [ Clause [VarP (mkName "r"), VarP (mkName "s")]
                 (NormalB $ VarE 'runEffTIn' `AppE`
                         VarE (mkName "r") `AppE`
                         VarE (mkName "s")
                 )
                 []
        ]
      runMyModuleIn_Fun = FunD runMyModuleIn_Name
        [ Clause [VarP (mkName "r"), VarP (mkName "s")]
                 (NormalB $ VarE 'runEffTIn_ `AppE`
                         VarE (mkName "r") `AppE`
                         VarE (mkName "s")
                 )
                 []
        ]

  lensDecsR <- concat <$> mapM (generateLens readType)  (dataFields readSpec)
  lensDecsS <- concat <$> mapM (generateLens stateType) (dataFields stateSpec)

  pure $ [ dataTag
         , instanceModule
         , typeSynRead
         , typeSynState
         , runMyModuleSig    , runMyModuleFun    , inlinePragma runMyModuleName
         , runMyModule'Sig   , runMyModule'Fun   , inlinePragma runMyModule'Name
         , runMyModule_Sig   , runMyModule_Fun   , inlinePragma runMyModule_Name
         , runMyModuleInSig  , runMyModuleInFun  , inlinePragma runMyModuleInName
         , runMyModuleIn'Sig , runMyModuleIn'Fun , inlinePragma runMyModuleIn'Name
         , runMyModuleIn_Sig , runMyModuleIn_Fun , inlinePragma runMyModuleIn_Name
         ] <> lensDecsR <> lensDecsS

generateRModule :: GenerationConfig -> DataConsSpec -> Q [Dec]
generateRModule GenerationConfig{ deriveConfigs = dconf, generateSystemInstance } DataConsSpec{dataConsName = modName, dataFields} = do
  let deriveGeneric = [deriveG  | ConfigDeriveGeneric `elem` dconf]
      deriveNFData  = [deriveNF | ConfigDeriveNFData  `elem` dconf]
  ---------------------------------------------------------------------------
  -- Helper names
  ---------------------------------------------------------------------------
  let readConName  = mkName $ nameBase modName ++ "Read"
      readTy       = ConT ''ModuleRead `AppT` ConT modName
      stateConName = mkName $ nameBase modName ++ "State"
      eventConName = mkName $ nameBase modName ++ "Event"
      initDataConName = mkName $ nameBase modName ++ "InitData"
      runName      = mkName $ "run" ++ nameBase modName
      runInName    = mkName $ "run" ++ nameBase modName ++ "In"

  ---------------------------------------------------------------------------
  -- data <MyModule>
  ---------------------------------------------------------------------------
  let tagDec = DataD [] modName [] Nothing [] []

  ---------------------------------------------------------------------------
  -- the associated data instances inside  `instance Module <MyModule>`
  --
  -- and `instance SystemModule <MyModule>`
  --
  -- when there is only one record, automatically switch to newtype instead
  ---------------------------------------------------------------------------
  let instanceModule = InstanceD Nothing [] (AppT (ConT ''Module) (ConT modName))
        [ dataInstance DataInstanceSpec
             { dataFamilyType        = ConT ''ModuleRead
             , dataFamilyInputType   = ConT modName
             , dataFamilyConstructor = DataConsSpec { dataConsName = readConName, dataFields = dataFields }
             , dataFamilyDerivations = deriveGeneric
             }
        , dataInstance DataInstanceSpec
             { dataFamilyType        = ConT ''ModuleState
             , dataFamilyInputType   = ConT modName
             , dataFamilyConstructor = DataConsSpec { dataConsName = stateConName, dataFields = [] }
             , dataFamilyDerivations = deriveGeneric <> deriveNFData
             }
        ]

  let instanceSystemModule = InstanceD Nothing [] (ConT ''SystemModule `AppT` ConT modName)
        [ dataInstance DataInstanceSpec
            { dataFamilyType        = ConT ''ModuleEvent
            , dataFamilyInputType   = ConT modName
            , dataFamilyConstructor = DataConsSpec { dataConsName = eventConName, dataFields = [] }
            , dataFamilyDerivations = deriveGeneric <> deriveNFData
            }
        , dataInstance DataInstanceSpec
            { dataFamilyType        = ConT ''ModuleInitData
            , dataFamilyInputType   = ConT modName
            , dataFamilyConstructor = DataConsSpec { dataConsName = initDataConName, dataFields = appendName "Init" <$> dataFields }
            , dataFamilyDerivations = deriveGeneric
            }
        ]

  ---------------------------------------------------------------------------
  -- run<MyModule>
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
  -- run<MyModule>In
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
        ForallT [ PlainTV modsTv SpecifiedSpec, PlainTV esTv SpecifiedSpec
                , PlainTV mTv SpecifiedSpec, PlainTV cTv SpecifiedSpec, PlainTV aTv SpecifiedSpec ]
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
  -- Type synonym for ModuleRead
  ---------------------------------------------------------------------------

  let typeSyn = TySynD readConName [] readTy

  pure $  [ tagDec
          , instanceModule
          ] <>
          [ instanceSystemModule | generateSystemInstance ]
          <>
          [ runSig , runFun , runPrag
          , runInSig , runInFun, runInPrag
          , typeSyn
          ]

-------------------------------------------------------------------------------
-- Helpers (TH arrow type)
-------------------------------------------------------------------------------
arr :: Type -> Type -> Type
arr = AppT . AppT ArrowT
infixr 5 `arr`
