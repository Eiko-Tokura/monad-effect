{-# LANGUAGE AllowAmbiguousTypes #-}
module Data.TypeList.FData.TH where

import Data.TypeList.Families (Nat(..), SNat(..))
import Language.Haskell.TH.Syntax hiding (Type)
import qualified Language.Haskell.TH.Syntax as TH
import Control.Monad (when)

class QClass (n :: Nat) where
  someQ :: Q Exp

instance QClass 'Zero where
  someQ = [| 0 |]

instance QClass n => QClass ('Succ n) where
  someQ = [| (+1) . $(someQ @n) |]

experiment :: forall (n :: Nat). SNat n -> (Exp -> Q Exp) -> (forall (k :: Nat). Exp -> SNat k -> Q Exp) -> Exp -> Q Exp
experiment SZero eZero _eSucc z     = eZero z
experiment (SSucc n) eZero eSucc z  = do
  expS <- eSucc z n
  experiment n eZero eSucc expS

experimentFunc :: SNat n -> Q Exp
experimentFunc n = experiment n
  (\e -> pure e)
  (\e (_ :: SNat k) -> [| (+1) . $(pure e) |])
  (VarE 'id)

generateFDataInstance :: Int -> Q Dec
generateFDataInstance n = do
  Just fdataName <- lookupTypeName "FData"
  -- Names for the parameters
  let fName  = mkName "f"
      tNames = [mkName ("t" ++ show i) | i <- [1 .. n]]

      fVar   = VarT fName
      tVars  = map VarT tNames

      -- Type-level list '[t1, …, tn]
      listTy = foldr (\t acc -> AppT (AppT PromotedConsT t) acc)
                     PromotedNilT
                     tVars

      -- Head of the instance:  FData f '[…]
      instHead = AppT (AppT (ConT fdataName) fVar) listTy

      -- Constructor name, e.g. FData3
      conName = mkName ("FData" ++ show n)

      -- Helper for individual record fields
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
           []              -- No deriving clauses

generateFDataInstances :: [Int] -> Q [Dec]
generateFDataInstances = mapM generateFDataInstance

zeroName, succName :: Name
zeroName = 'Zero
succName = 'Succ

--------------------------------------------------------------------------------
-- helpers ---------------------------------------------------------------------
--------------------------------------------------------------------------------

-- Promoted Nat at the type level: 0 -> 'Zero, 3 -> 'Succ ('Succ ('Succ 'Zero))
natTy :: Int -> TH.Type
natTy 0 = PromotedT zeroName
natTy n = AppT (PromotedT succName) (natTy (n-1))

-- A promoted type-level list '[t1 … tn]
promotedListTy :: [TH.Type] -> TH.Type
promotedListTy = foldr (\t acc -> AppT (AppT PromotedConsT t) acc) PromotedNilT

-- Field patterns for “get” (only the target gets a variable, others are _)
mkGetPats :: Int -> Int -> Name -> [Pat]
mkGetPats len idx var =
  [ if i == idx then VarP var else WildP | i <- [0 .. len-1] ]

-- Field patterns for “modify” (every field bound to xi)
mkModPats :: Int -> [Name] -> [Pat]
mkModPats len vars = [ VarP (vars !! i) | i <- [0 .. len-1] ]

--------------------------------------------------------------------------------
-- | @generateFDataByIndexInstance idx len@
--   produces
--   > instance FDataByIndex (Nat idx) '[x1 … xlen] where …
generateFDataByIndexInstance :: Int -> Int -> Q Dec
generateFDataByIndexInstance idx len = do
  when (idx < 0 || idx >= len) $
    fail $ "generateFDataByIndexInstance: index " ++ show idx
        ++ " out of bounds for list length " ++ show len

  ----------------------------------------------------------------------
  -- names -------------------------------------------------------------
  ----------------------------------------------------------------------
  let conName   = mkName ("FData" ++ show len)          -- FData1 .. FData5 …
      className = mkName "FDataByIndex"                 --''FDataByIndex
      fName     = mkName "f"                            -- the function arg
      -- x1 .. xn  (term vars)
      xNames    = [ mkName ("x" ++ show i) | i <- [1 .. len] ]
      xiName    = xNames !! idx                         -- xi (to be focused)
      -- x1 .. xn  (type vars)
      tNames    = [ mkName ("x" ++ show i) | i <- [1 .. len] ]
      tVars     = map VarT tNames

      fGetFDataByIndex = mkName "getFDataByIndex"    -- getFDataByIndex
      fModifyFDataByIndex  = mkName "modifyFDataByIndex" -- modifyFDataByIndex

  ----------------------------------------------------------------------
  -- instance head -----------------------------------------------------
  ----------------------------------------------------------------------
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
  -- finished instance -------------------------------------------------
  ----------------------------------------------------------------------
  pure $ InstanceD
          Nothing           -- no overlap pragmas
          []                -- no constraints
          instHead
          [ FunD fGetFDataByIndex    [getClause]
          , FunD fModifyFDataByIndex [modifyClause]
          , PragmaD (InlineP fGetFDataByIndex Inline FunLike AllPhases)
          , PragmaD (InlineP fModifyFDataByIndex Inline FunLike AllPhases)
          ]

generateFDataByIndexInstances :: [(Int, Int)] -> Q [Dec]
generateFDataByIndexInstances = mapM (uncurry generateFDataByIndexInstance)
