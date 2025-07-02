module Data.FData
  ( FData(..), FDataByIndex(..)
  , module Data.FData
  ) where

import Data.HList
import Data.FData.TH
import Data.Kind (Type)
import Data.Type.Equality

data family FData (f :: Type -> Type) (ts :: [Type]) :: Type
data instance FData f '[] = FData0

type In' e ts = (In e ts, FDataByIndex (FirstIndex e ts) ts)

getFData :: forall e ts f. (In' e ts) => FData f ts -> f e
getFData = case proofIndex @e @ts of Refl -> getFDataByIndex (singIndex @e @ts)
{-# INLINE getFData #-}

modifyFData :: forall e ts f. (In' e ts) => (f e -> f e) -> FData f ts -> FData f ts
modifyFData f = case proofIndex @e @ts of Refl -> modifyFDataByIndex (singIndex @e @ts) f
{-# INLINE modifyFData #-}

class FDataByIndex (n :: Nat) (ts :: [Type]) where
  getFDataByIndex    :: SNat n -> FData f ts -> f (AtIndex ts n)
  modifyFDataByIndex :: SNat n -> (f (AtIndex ts n) -> f (AtIndex ts n)) -> FData f ts -> FData f ts

$(generateFDataInstances [1..16])

$(generateFDataByIndexInstances [(j, i) | i <- [1..16], j <- [0..i-1]])

-- data instance FData f '[t] = FData1 { fdata1_0 :: !(f t) }
-- data instance FData f '[t1, t2] = FData2 { fdata2_0 :: !(f t1), fdata2_1 :: !(f t2) }
-- data instance FData f '[t1, t2, t3] = FData3 { fdata3_0 :: !(f t1), fdata3_1 :: !(f t2), fdata3_2 :: !(f t3) }
-- data instance FData f '[t1, t2, t3, t4] = FData4
--   { fdata4_0 :: !(f t1)
--   , fdata4_1 :: !(f t2)
--   , fdata4_2 :: !(f t3)
--   , fdata4_3 :: !(f t4)
--   }
-- data instance FData f '[t1, t2, t3, t4, t5] = FData5
--   { fdata5_0 :: !(f t1)
--   , fdata5_1 :: !(f t2)
--   , fdata5_2 :: !(f t3)
--   , fdata5_3 :: !(f t4)
--   , fdata5_4 :: !(f t5)
--   }

-- DataInstD Cxt (Maybe [TyVarBndr ()]) Type (Maybe Kind) [Con] [DerivClause]	
-- { data instance Cxt x => T [x]
--     = A x | B (T x)
--     deriving (Z,W)
--     deriving stock Eq }

-- $(generateFDataInstance 0)
-- data FData (f :: Type -> Type) (ts :: [Type]) where
--   FData0 :: FData f '[]
--   FData1 :: { fdata1_0 :: !(f t) } -> FData f '[t]
--   FData2 :: { fdata2_0 :: !(f t1), fdata2_1 :: !(f t2) } -> FData f '[t1, t2]
--   FData3 :: { fdata3_0 :: !(f t1), fdata3_1 :: !(f t2), fdata3_2 :: !(f t3) } -> FData f '[t1, t2, t3]
--   FData4 ::
--     { fdata4_0 :: !(f t1)
--     , fdata4_1 :: !(f t2)
--     , fdata4_2 :: !(f t3)
--     , fdata4_3 :: !(f t4) 
--     } -> FData f '[t1, t2, t3, t4]
--   FData5 ::
--     { fdata5_0 :: !(f t1)
--     , fdata5_1 :: !(f t2)
--     , fdata5_2 :: !(f t3)
--     , fdata5_3 :: !(f t4)
--     , fdata5_4 :: !(f t5)
--     } -> FData f '[t1, t2, t3, t4, t5]

-- class In e ts => FDataAccess (e :: Type) (ts :: [Type]) where
--   getFData    :: FData f ts -> f e
--   modifyFData :: (f e -> f e) -> FData f ts -> FData f ts

-- instance FDataByIndex Zero '[x1] where
--   getFDataByIndex    _   (FData1 x) = x
--   modifyFDataByIndex _ f (FData1 x) = FData1 (f x)
--   {-# INLINE getFDataByIndex #-}
--   {-# INLINE modifyFDataByIndex #-}
-- instance FDataByIndex Zero '[x1, x2] where
--   getFDataByIndex    _   (FData2 x1 _ ) = x1
--   modifyFDataByIndex _ f (FData2 x1 x2) = FData2 (f x1) x2
--   {-# INLINE getFDataByIndex #-}
--   {-# INLINE modifyFDataByIndex #-}
-- 
-- instance FDataByIndex (Succ Zero) '[x1, x2] where
--   getFDataByIndex    _ (FData2 _ x2) = x2
--   modifyFDataByIndex _ f (FData2 x1 x2) = FData2 x1 (f x2)
--   {-# INLINE getFDataByIndex #-}
--   {-# INLINE modifyFDataByIndex #-}
-- 
-- instance FDataByIndex Zero '[x1, x2, x3] where
--   getFDataByIndex    _   (FData3 x1 _ _) = x1
--   modifyFDataByIndex _ f (FData3 x1 x2 x3) = FData3 (f x1) x2 x3
--   {-# INLINE getFDataByIndex #-}
--   {-# INLINE modifyFDataByIndex #-}
-- 
-- instance FDataByIndex Zero '[x1, x2, x3, x4] where
--   getFDataByIndex    _   (FData4 x1 _ _ _) = x1
--   modifyFDataByIndex _ f (FData4 x1 x2 x3 x4) = FData4 (f x1) x2 x3 x4
--   {-# INLINE getFDataByIndex #-}
--   {-# INLINE modifyFDataByIndex #-}
-- 
-- instance FDataByIndex Zero '[x1, x2, x3, x4, x5] where
--   getFDataByIndex    _   (FData5 x1 _ _ _ _) = x1
--   modifyFDataByIndex _ f (FData5 x1 x2 x3 x4 x5) = FData5 (f x1) x2 x3 x4 x5
--   {-# INLINE getFDataByIndex #-}
--   {-# INLINE modifyFDataByIndex #-}
