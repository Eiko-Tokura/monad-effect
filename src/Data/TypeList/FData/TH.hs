{-# LANGUAGE AllowAmbiguousTypes, ViewPatterns #-}
-- | This module provides Template Haskell utilities for generating instances of FData, including
-- FDataByIndex, UnConsFData, ConsFData0, ConsFData1, and RemoveElem.
--
-- This module is internal
module Data.TypeList.FData.TH where

import Data.TypeList.Families (Nat(..))
import Language.Haskell.TH.Syntax hiding (Type)
import qualified Language.Haskell.TH.Syntax as TH
import Control.Monad (when)

generateFDataInstance :: Int -> Q Dec
generateFDataInstance n = do
  Just fdataName <- lookupTypeName "FData"
  let fName  = mkName "f"
      tNames = [mkName ("t" ++ show i) | i <- [1 .. n]]

      fVar   = VarT fName
      tVars  = map VarT tNames

      listTy = foldr (\t acc -> AppT (AppT PromotedConsT t) acc)
                     PromotedNilT
                     tVars

      instHead = AppT (AppT (ConT fdataName) fVar) listTy

      conName = mkName ("FData" ++ show n)

      mkField :: Int -> TH.Type -> (Name, Bang, TH.Type)
      mkField idx t =
        ( mkName ("fdata" ++ show n ++ "_" ++ show idx)
        , Bang NoSourceUnpackedness SourceStrict
        , AppT fVar t
        )

      -- Chosen constructor (record for n>0, plain for n==0)
      con | n == 0  = NormalC conName []
          | otherwise =
              RecC conName (zipWith mkField [0 ..] tVars)

  return $ DataInstD
           []              -- No context
           Nothing         -- No explicit binders
           instHead
           Nothing         -- Kind signature*
           [con]
           [ DerivClause (Just StockStrategy)    $ ConT <$> [mkName "Generic"]
           , DerivClause (Just AnyclassStrategy) $ ConT <$> [mkName "NFData"]
           , DerivClause (Just AnyclassStrategy) $ ConT <$> [mkName "Default"]
           ]              -- No deriving clauses

generateFDataInstances :: [Int] -> Q [Dec]
generateFDataInstances = mapM generateFDataInstance

--------------------------------------------------------------------------------
-- helpers ---------------------------------------------------------------------
--------------------------------------------------------------------------------

zeroName, succName :: Name
zeroName = 'Zero
succName = 'Succ

fDataCon :: Int -> Name
fDataCon n = mkName $ "FData" ++ show n

-- Promoted Nat at the type level: 0 -> 'Zero, 3 -> 'Succ ('Succ ('Succ 'Zero))
natTy :: Int -> TH.Type
natTy 0 = PromotedT zeroName
natTy n = AppT (PromotedT succName) (natTy (n-1))

-- A promoted type-level list '[t1 ... tn]
promotedListTy :: [TH.Type] -> TH.Type
promotedListTy = foldr (\t acc -> AppT (AppT PromotedConsT t) acc) PromotedNilT

-- Type names for x1, x2, ..., xn
xTypeNames :: [Int] -> [Name]
xTypeNames = map (\i -> mkName ("x" ++ show i))

-- Field patterns for “get” (only the target gets a variable, others are _)
mkGetPats :: Int -> Int -> Name -> [Pat]
mkGetPats len idx var =
  [ if i == idx then VarP var else WildP | i <- [0 .. len-1] ]

-- Field patterns for “modify” (every field bound to xi)
mkModPats :: Int -> [Name] -> [Pat]
mkModPats len vars = [ VarP (vars !! i) | i <- [0 .. len-1] ]

inline :: Name -> Pragma
inline f = InlineP f Inline FunLike AllPhases

--------------------------------------------------------------------------------
-- | @generateFDataByIndexInstance idx len@
--   produces
--   > instance FDataByIndex (Nat idx) '[x1 .. xlen]
generateFDataByIndexInstance :: Int -> Int -> Q Dec
generateFDataByIndexInstance idx len = do
  when (idx < 0 || idx >= len) $
    fail $ "generateFDataByIndexInstance: index " ++ show idx
        ++ " out of bounds for list length " ++ show len

  let conName   = mkName ("FData" ++ show len)          -- FData1 .. FData5 …
      className = mkName "FDataByIndex"                 --''FDataByIndex
      fName     = mkName "f"                            -- the function arg
      gName     = mkName "g"                            -- functorial updater
      -- x1 .. xn  (term vars)
      xNames    = [ mkName ("x" ++ show i) | i <- [1 .. len] ]
      xiName    = xNames !! idx                         -- xi (to be focused)
      xiPrime   = mkName (nameBase xiName ++ "'")       -- updated slot
      -- x1 .. xn  (type vars)
      tNames    = [ mkName ("x" ++ show i) | i <- [1 .. len] ]
      tVars     = map VarT tNames

      fGetFDataByIndex = mkName "getFDataByIndex"    -- getFDataByIndex
      fModifyFDataByIndex  = mkName "modifyFDataByIndex" -- modifyFDataByIndex
      fLensFDataByIndex    = mkName "lensFDataByIndex"

  let instHead = AppT
                   (AppT (ConT className) (natTy idx))
                   (promotedListTy tVars)

  ----------------------------------------------------------------------
  -- method: getFDataByIndex ------------------------------------------
  ----------------------------------------------------------------------
      getClause =
        Clause [ WildP
               , ConP conName [] (mkGetPats len idx xiName)
               ]
               (NormalB (VarE xiName))
               []

  ----------------------------------------------------------------------
  -- method: modifyFDataByIndex ---------------------------------------
  ----------------------------------------------------------------------
      patVars  = mkModPats len xNames      -- (FDataN x1 .. xn)
      fieldExs = [ if i == idx
                     then AppE (VarE fName) (VarE (xNames !! i))
                     else VarE (xNames !! i)
                 | i <- [0 .. len-1] ]

      modifyBody = foldl AppE (ConE conName) fieldExs

      modifyClause =
        Clause [ WildP
               , VarP fName
               , ConP conName [] patVars
               ]
               (NormalB modifyBody)
               []
  ----------------------------------------------------------------------
  -- lensFDataByIndex
  --   lensFDataByIndex _ g (FDataN x1 .. xi .. xn)
  --     = fmap (\xi' -> FDataN x1 .. xi' .. xn) (g xi)
  ----------------------------------------------------------------------
      gCall      = AppE (VarE gName) (VarE xiName)

      rebuiltFields =
        [ if i == idx then VarE xiPrime else VarE (xNames !! i)
        | i <- [0 .. len-1] ]

      rebuildWithXi' = LamE [VarP xiPrime]
                         (foldl AppE (ConE conName) rebuiltFields)

      lensBody = AppE (AppE (VarE 'fmap) rebuildWithXi') gCall

      lensClause =
        Clause [ WildP
               , VarP gName
               , ConP conName [] patVars
               ]
               (NormalB lensBody)
               []

  pure $ InstanceD
          Nothing
          []
          instHead
          [ FunD fGetFDataByIndex    [getClause]
          , FunD fModifyFDataByIndex [modifyClause]
          , FunD fLensFDataByIndex   [lensClause]
          , PragmaD $ inline fGetFDataByIndex
          , PragmaD $ inline fModifyFDataByIndex
          , PragmaD $ inline fLensFDataByIndex
          ]
  -- pure $ InstanceD
  --         Nothing
  --         []
  --         instHead
  --         [ FunD fGetFDataByIndex    [getClause]
  --         , FunD fModifyFDataByIndex [modifyClause]
  --         , PragmaD $ inline fGetFDataByIndex
  --         , PragmaD $ inline fModifyFDataByIndex
  --         ]

generateFDataByIndexInstances :: [(Int, Int)] -> Q [Dec]
generateFDataByIndexInstances = mapM (uncurry generateFDataByIndexInstance)

-- | @generateUnconsFDataInstance n@ produces an instance of UnConsFData FData '[x1, ..., xn]
generateUnconsFDataInstance :: Int -> Q Dec
generateUnconsFDataInstance ((<1) -> True) = fail "generateUnconsFDataInstance: length must be at least 1"
generateUnconsFDataInstance n = do
  let className      = mkName "UnConsFData"
      fData          = mkName "FData"
      fDataNConName  = fDataCon n
      fDataN'ConName = fDataCon (n-1)
      xNames = xTypeNames [1 .. n]
      x1Name  = mkName "x1"
      fUnConsFData = mkName "unConsFData" -- unConsFData
  pure $ InstanceD Nothing [] (ConT className `AppT` ConT fData `AppT` promotedListTy (VarT <$> xNames))
    [ FunD
      fUnConsFData
      [ Clause
        [ ConP fDataNConName [] (VarP <$> xNames) ]
        (NormalB $ TupE
            [ Just (VarE x1Name)
            , Just $ foldl' AppE (ConE fDataN'ConName) (VarE <$> drop 1 xNames)
            ]
        )
        []
      ]
    , PragmaD $ inline fUnConsFData
    ]
    
generateUnconsFDataInstances :: [Int] -> Q [Dec]
generateUnconsFDataInstances = mapM generateUnconsFDataInstance

-- | @generateConsFData0Instance n@ produces an instance of ConsFData0 FData '[x1, ..., xn]
generateConsFData0Instance :: Int -> Q Dec
generateConsFData0Instance ((<1) -> True) = fail "generateConsFData0Instance: length must be at least 1"
generateConsFData0Instance n = do
  let className      = mkName "ConsFData0"
      fData          = mkName "FData"
      fDataNConName  = fDataCon n
      fDataN'ConName = fDataCon (n-1)
      xTyNames = xTypeNames [1 .. n]
      xNames = xTypeNames [1 .. n-1]
      xName  = mkName "x"
      fConsFData0 = mkName "consF0" -- consF0
  pure $ InstanceD Nothing [] (ConT className `AppT` ConT fData `AppT` promotedListTy (VarT <$> xTyNames))
    [ FunD
      fConsFData0
      [ Clause
        [ VarP xName
        , ConP fDataN'ConName [] (VarP <$> xNames)
        ]
        (NormalB $ foldl' AppE (ConE fDataNConName) (VarE xName : fmap VarE xNames))
        []
      ]
    , PragmaD $ inline fConsFData0
    ]

generateConsFData0Instances :: [Int] -> Q [Dec]
generateConsFData0Instances = mapM generateConsFData0Instance

-- | @generateConsFData1Instance n@ produces an instance of ConsFData1 FData '[x1, ..., xn]
generateConsFData1Instance :: Int -> Q Dec
generateConsFData1Instance ((<0) -> True) = fail "generateConsFData1Instance: length must be at least 0"
generateConsFData1Instance n = do
  let className      = mkName "ConsFData1"
      fData          = mkName "FData"
      fDataNConName  = fDataCon n
      fDataN'ConName = fDataCon (n+1)
      xTyNames = xTypeNames [1 .. n]
      xNames = xTypeNames [1 .. n]
      xName  = mkName "x"
      fConsFData1 = mkName "consF1" -- consF1
  pure $ InstanceD Nothing [] (ConT className `AppT` ConT fData `AppT` promotedListTy (VarT <$> xTyNames))
    [ FunD
      fConsFData1
      [ Clause
        [ VarP xName
        , ConP fDataNConName [] (VarP <$> xNames)
        ]
        (NormalB $ foldl' AppE (ConE fDataN'ConName) (VarE xName : fmap VarE xNames))
        []
      ]
    , PragmaD $ inline fConsFData1
    ]

generateConsFData1Instances :: [Int] -> Q [Dec]
generateConsFData1Instances = mapM generateConsFData1Instance

-- | @generateRemoveElemInstance n@ produces an instance of RemoveElem FData '[x1, ..., xn]
generateRemoveElemInstance :: Int -> Q Dec
generateRemoveElemInstance ((<1) -> True) = fail "generateRemoveElemInstance: length must be at least 1"
generateRemoveElemInstance n = do
  when (n < 1) $
    fail "generateRemoveElemInstance: list length must be ≥ 1"

  let tNames = xTypeNames [1 .. n]
      tVars  = map VarT tNames
      listTy = promotedListTy tVars

      fRemoveElem = mkName "removeElem"    -- removeElem
      cRemoveElem = mkName "RemoveElem"    -- RemoveElem, the class name
      fUnRemoveElem = mkName "unRemoveElem" -- unRemoveElem

      instHead = ConT cRemoveElem `AppT` ConT (mkName "FData") `AppT` listTy

      clausesRemove  = [ mkRemoveClause  i  | i <- [0 .. n-1] ]
      clausesUnRem   = [ mkUnRemoveClause i | i <- [0 .. n-1] ]

      mkRemoveClause i =
        let conN     = fDataCon n
            xs       = xTypeNames [1 .. n]  -- x1, x2, ..., xn
            patSFirstIndex = sFirstIndexPat i
            patFData = ConP conN [] $
                         [ if j == i then WildP else VarP (xs !! j)
                         | j <- [0 .. n-1] ]
            result   = foldl AppE (ConE (fDataCon (n-1)))
                                   [ VarE (xs !! j) | j <- [0 .. n-1], j /= i ]
        in Clause [patSFirstIndex, patFData] (NormalB result) []

      mkUnRemoveClause i =
        let xNew     = mkName "x"          -- the element being re-inserted
            ys       = xTypeNames [1 .. n-1]  -- x1, x2, ..., xn-1
            tailCon  = fDataCon (n-1)
            patSFirstIndex = sFirstIndexPat i
            patTail  = ConP tailCon [] (map VarP ys)

            -- rebuild FData<n> with x inserted at position i
            fields = [ if j == i
                         then VarE xNew
                         else VarE (ys !! (if j < i then j else j-1))
                     | j <- [0 .. n-1] ]
            result = foldl AppE (ConE (fDataCon n)) fields
        in Clause [patSFirstIndex, VarP xNew, patTail] (NormalB result) []

  pure $ InstanceD Nothing [] instHead
          [ FunD fRemoveElem    clausesRemove
          , FunD fUnRemoveElem  clausesUnRem
          , PragmaD $ inline fRemoveElem
          , PragmaD $ inline fUnRemoveElem
          ]
    where
      sFirstIndexPat :: Int -> Pat
      sFirstIndexPat 0 = ConP (mkName "SFirstIndexZero") [] []
      sFirstIndexPat i =
        ConP (mkName "SFirstIndexSucc") [] [ ConP (mkName "Refl") [] [] , sFirstIndexPat (i-1) ]

generateRemoveElemInstances :: [Int] -> Q [Dec]
generateRemoveElemInstances = mapM generateRemoveElemInstance
