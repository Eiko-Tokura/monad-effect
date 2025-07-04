{-# LANGUAGE UndecidableInstances #-}
module Data.TypeList.FData
  ( FData(..), FDataByIndex(..)
  , module Data.TypeList.FData
  , module Data.TypeList
  ) where

import Data.TypeList
import Data.TypeList.FData.TH
import Data.Kind (Type)
import Data.Type.Equality

data family FData (f :: Type -> Type) (ts :: [Type]) :: Type
data instance FData f '[] = FData0

type instance FDataConstraint FData e ts = (FDataByIndex (FirstIndex e ts) ts)

-- type In' e ts = (In e ts, FDataByIndex (FirstIndex e ts) ts)
getFData :: forall e ts f. (In e ts, FDataConstraint FData e ts) => FData f ts -> f e
getFData = case proofIndex @e @ts of Refl -> getFDataByIndex (singIndex @e @ts)
{-# INLINE getFData #-}

modifyFData :: forall e ts f. (In e ts, FDataConstraint FData e ts) => (f e -> f e) -> FData f ts -> FData f ts
modifyFData f = case proofIndex @e @ts of Refl -> modifyFDataByIndex (singIndex @e @ts) f
{-# INLINE modifyFData #-}

instance (FDataConstraint FData e es, In e es) => In' FData e es where
  getIn = getFData
  modifyIn = modifyFData

class FDataByIndex (n :: Nat) (ts :: [Type]) where
  -- | This worked because it does not depend on SNat, just used it to pass the type parameter.
  getFDataByIndex    :: SNat n -> FData f ts -> f (AtIndex ts n)
  modifyFDataByIndex :: SNat n -> (f (AtIndex ts n) -> f (AtIndex ts n)) -> FData f ts -> FData f ts

$(generateFDataInstances [1..16])

instance RemoveElem FData '[x1] where
  removeElem SFirstIndexZero (FData1 _x1) = FData0
  {-# INLINE removeElem #-}

  unRemoveElem SFirstIndexZero x (FData0) = FData1 x
  {-# INLINE unRemoveElem #-}

instance RemoveElem FData '[x1, x2] where
  removeElem SFirstIndexZero (FData2 _x1 x2)                        = FData1 x2
  removeElem (SFirstIndexSucc Refl SFirstIndexZero) (FData2 x1 _x2) = FData1 x1
  {-# INLINE removeElem #-}

  unRemoveElem SFirstIndexZero x (FData1 x2)                        = FData2 x x2
  unRemoveElem (SFirstIndexSucc Refl SFirstIndexZero) x (FData1 x1) = FData2 x1 x
  {-# INLINE unRemoveElem #-}

instance RemoveElem FData '[x1, x2, x3] where
  removeElem SFirstIndexZero (FData3 _x1 x2 x3)                                               = FData2 x2 x3
  removeElem (SFirstIndexSucc Refl SFirstIndexZero) (FData3 x1 _x2 x3)                        = FData2 x1 x3
  removeElem (SFirstIndexSucc Refl (SFirstIndexSucc Refl SFirstIndexZero)) (FData3 x1 x2 _x3) = FData2 x1 x2
  {-# INLINE removeElem #-}

  unRemoveElem SFirstIndexZero x (FData2 x2 x3)                                               = FData3 x x2 x3
  unRemoveElem (SFirstIndexSucc Refl SFirstIndexZero) x (FData2 x1 x3)                        = FData3 x1 x x3
  unRemoveElem (SFirstIndexSucc Refl (SFirstIndexSucc Refl SFirstIndexZero)) x (FData2 x1 x2) = FData3 x1 x2 x
  {-# INLINE unRemoveElem #-}

instance
  ( flist ~ FData
  , WhenNonEmpty ts (ConsFDataList flist (Tail ts))
  , ConsFNil flist
  , WhenNonEmpty ts (UnConsFData flist ts)
  , WhenNonEmpty ts (ConsFData0 flist ts)
  , ConsFData1 flist ts
  ) => ConsFDataList FData ts 

instance ConsFNil FData where
  fNil = FData0
  {-# INLINE fNil #-}

instance UnConsFData FData '[x1] where
  unConsFData (FData1 x1) = (x1, fNil)
  {-# INLINE unConsFData #-}
instance UnConsFData FData '[x1, x2] where
  unConsFData (FData2 x1 x2) = (x1, FData1 x2)
  {-# INLINE unConsFData #-}
instance UnConsFData FData '[x1, x2, x3] where
  unConsFData (FData3 x1 x2 x3) = (x1, FData2 x2 x3)
  {-# INLINE unConsFData #-}
instance UnConsFData FData '[x1, x2, x3, x4] where
  unConsFData (FData4 x1 x2 x3 x4) = (x1, FData3 x2 x3 x4)
  {-# INLINE unConsFData #-}
instance UnConsFData FData '[x1, x2, x3, x4, x5] where
  unConsFData (FData5 x1 x2 x3 x4 x5) = (x1, FData4 x2 x3 x4 x5)
  {-# INLINE unConsFData #-}

instance ConsFData0 FData '[x1] where
  consF0 x (FData0) = FData1 x
  {-# INLINE consF0 #-}
instance ConsFData0 FData '[x1, x2] where
  consF0 x (FData1 x1) = FData2 x x1
  {-# INLINE consF0 #-}
instance ConsFData0 FData '[x1, x2, x3] where
  consF0 x (FData2 x1 x2) = FData3 x x1 x2
  {-# INLINE consF0 #-}
instance ConsFData0 FData '[x1, x2, x3, x4] where
  consF0 x (FData3 x1 x2 x3) = FData4 x x1 x2 x3
  {-# INLINE consF0 #-}
instance ConsFData0 FData '[x1, x2, x3, x4, x5] where
  consF0 x (FData4 x1 x2 x3 x4) = FData5 x x1 x2 x3 x4
  {-# INLINE consF0 #-}

instance ConsFData1 FData '[] where
  consF1 x (FData0) = FData1 x
  {-# INLINE consF1 #-}
instance ConsFData1 FData '[x1] where
  consF1 x (FData1 x1) = FData2 x x1
  {-# INLINE consF1 #-}
instance ConsFData1 FData '[x1, x2] where
  consF1 x (FData2 x1 x2) = FData3 x x1 x2
  {-# INLINE consF1 #-}
instance ConsFData1 FData '[x1, x2, x3] where
  consF1 x (FData3 x1 x2 x3) = FData4 x x1 x2 x3
  {-# INLINE consF1 #-}
instance ConsFData1 FData '[x1, x2, x3, x4] where
  consF1 x (FData4 x1 x2 x3 x4) = FData5 x x1 x2 x3 x4
  {-# INLINE consF1 #-}

------

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
