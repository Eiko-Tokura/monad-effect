{-# LANGUAGE UndecidableInstances, DeriveAnyClass #-}
-- | The `FData` type family is a fast replacement for `FList`, the heterogeneous list.
-- it builds a data structure using data family instead of GADT, which is very efficient
--
-- instances are generated using Template Haskell for up to 20 elements.
--
-- This module is considered INTERNAL, you can use it but be aware that the API may change without major version bumps.
module Data.TypeList.FData
  ( FData(..), FDataByIndex(..)
  , module Data.TypeList
  ) where

import Control.DeepSeq (NFData(..))
import Data.Default
import Data.Kind (Type)
import Data.Proxy
import Data.Type.Equality
import Data.TypeList
import Data.TypeList.FData.TH
import GHC.Generics (Generic)

data family FData (f :: k -> Type) (ts :: [k]) :: Type
data instance FData f '[] = FData0

$(generateFDataInstances [1..20])

type instance FDataConstraint FData e ts = (FDataByIndex (FirstIndex e ts) ts)

instance (FDataConstraint FData e es, InList e es) => In' FData e es where
  getIn      = case proofIndex @e @es of Refl -> getFDataByIndex    (Proxy @(FirstIndex e es))
  modifyIn f = case proofIndex @e @es of Refl -> modifyFDataByIndex (Proxy @(FirstIndex e es)) f
  lensIn   f = case proofIndex @e @es of Refl -> lensFDataByIndex   (Proxy @(FirstIndex e es)) f
  {-# INLINE getIn #-}
  {-# INLINE modifyIn #-}
  {-# INLINE lensIn #-}

class FDataByIndex (n :: Nat) (ts :: [Type]) where
  getFDataByIndex    :: Proxy n -> FData f ts -> f (AtIndex ts n)
  modifyFDataByIndex :: Proxy n -> (f (AtIndex ts n) -> f (AtIndex ts n)) -> FData f ts -> FData f ts
  lensFDataByIndex   :: Proxy n
                     -> forall fun. (Functor fun)
                     => ( f (AtIndex ts n) -> fun (f (AtIndex ts n)) )
                     -> FData f ts -> fun (FData f ts)

instance
  ( WhenNonEmpty ts (ConsFDataList FData (Tail ts))
  , ConsFNil FData
  , WhenNonEmpty ts (UnConsFData FData ts)
  , WhenNonEmpty ts (ConsFData0 FData ts)
  , ConsFData1 FData ts
  ) => ConsFDataList FData ts 

instance ConsFNil FData where
  fNil = FData0
  {-# INLINE fNil #-}

$(generateUnconsFDataInstances [1..20])
$(generateConsFData0Instances [1..20])
$(generateConsFData1Instances [0..19])
$(generateRemoveElemInstances [1..20])
$(generateFDataByIndexInstances [(j, i) | i <- [1..20], j <- [0..i-1]])

-- for reference, the generated instances look like this:
--
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

-- class InList e ts => FDataAccess (e :: Type) (ts :: [Type]) where
--   getFData    :: FData f ts -> f e
--   modifyFData :: (f e -> f e) -> FData f ts -> FData f ts

-- instance FDataByIndex Zero '[x1] where
--   getFDataByIndex    _   (FData1 x) = x
--   modifyFDataByIndex _ f (FData1 x) = FData1 (f x)
--   {-# INLINE getFDataByIndex #-}
--   {-# INLINE modifyFDataByIndex #-}
--   lensFDataByIndex _ g (FData1 x) = fmap FData1 (g x)
--   {-# INLINE lensFDataByIndex #-}
--
-- instance FDataByIndex Zero '[x1, x2] where
--   getFDataByIndex    _   (FData2 x1 _ ) = x1
--   modifyFDataByIndex _ f (FData2 x1 x2) = FData2 (f x1) x2
--   {-# INLINE getFDataByIndex #-}
--   {-# INLINE modifyFDataByIndex #-}
--   lensFDataByIndex _ g (FData2 x1 x2) = fmap (\x1' -> FData2 x1' x2) (g x1)
--   {-# INLINE lensFDataByIndex #-}
-- 
-- instance FDataByIndex (Succ Zero) '[x1, x2] where
--   getFDataByIndex    _ (FData2 _ x2) = x2
--   modifyFDataByIndex _ f (FData2 x1 x2) = FData2 x1 (f x2)
--   {-# INLINE getFDataByIndex #-}
--   {-# INLINE modifyFDataByIndex #-}
--   lensFDataByIndex _ g (FData2 x1 x2) = fmap (\x2' -> FData2 x1 x2') (g x2)
--   {-# INLINE lensFDataByIndex #-}
-- 
-- instance FDataByIndex Zero '[x1, x2, x3] where
--   getFDataByIndex    _   (FData3 x1 _ _) = x1
--   modifyFDataByIndex _ f (FData3 x1 x2 x3) = FData3 (f x1) x2 x3
--   {-# INLINE getFDataByIndex #-}
--   {-# INLINE modifyFDataByIndex #-}
--   lensFDataByIndex _ g (FData3 x1 x2 x3) = fmap (\x1' -> FData3 x1' x2 x3) (g x1)
--   {-# INLINE lensFDataByIndex #-}
-- 
-- instance FDataByIndex Zero '[x1, x2, x3, x4] where
--   getFDataByIndex    _   (FData4 x1 _ _ _) = x1
--   modifyFDataByIndex _ f (FData4 x1 x2 x3 x4) = FData4 (f x1) x2 x3 x4
--   {-# INLINE getFDataByIndex #-}
--   {-# INLINE modifyFDataByIndex #-}
--   lensFDataByIndex _ g (FData4 x1 x2 x3 x4) = fmap (\x1' -> FData4 x1' x2 x3 x4) (g x1)
--   {-# INLINE lensFDataByIndex #-}
-- 
-- instance FDataByIndex Zero '[x1, x2, x3, x4, x5] where
--   getFDataByIndex    _   (FData5 x1 _ _ _ _) = x1
--   modifyFDataByIndex _ f (FData5 x1 x2 x3 x4 x5) = FData5 (f x1) x2 x3 x4 x5
--   {-# INLINE getFDataByIndex #-}
--   {-# INLINE modifyFDataByIndex #-}
--   lensFDataByIndex _ g (FData5 x1 x2 x3 x4 x5) = fmap (\x1' -> FData5 x1' x2 x3 x4 x5) (g x1)
--   {-# INLINE lensFDataByIndex #-}
--
-- instance UnConsFData FData '[x1] where
--   unConsFData (FData1 x1) = (x1, FData0)
--   {-# INLINE unConsFData #-}
-- instance UnConsFData FData '[x1, x2] where
--   unConsFData (FData2 x1 x2) = (x1, FData1 x2)
--   {-# INLINE unConsFData #-}
-- instance UnConsFData FData '[x1, x2, x3] where
--   unConsFData (FData3 x1 x2 x3) = (x1, FData2 x2 x3)
--   {-# INLINE unConsFData #-}
-- instance UnConsFData FData '[x1, x2, x3, x4] where
--   unConsFData (FData4 x1 x2 x3 x4) = (x1, FData3 x2 x3 x4)
--   {-# INLINE unConsFData #-}
-- instance UnConsFData FData '[x1, x2, x3, x4, x5] where
--   unConsFData (FData5 x1 x2 x3 x4 x5) = (x1, FData4 x2 x3 x4 x5)
--   {-# INLINE unConsFData #-}
-- 
-- instance ConsFData0 FData '[x1] where
--   consF0 x (FData0) = FData1 x
--   {-# INLINE consF0 #-}
-- instance ConsFData0 FData '[x1, x2] where
--   consF0 x (FData1 x1) = FData2 x x1
--   {-# INLINE consF0 #-}
-- instance ConsFData0 FData '[x1, x2, x3] where
--   consF0 x (FData2 x1 x2) = FData3 x x1 x2
--   {-# INLINE consF0 #-}
-- instance ConsFData0 FData '[x1, x2, x3, x4] where
--   consF0 x (FData3 x1 x2 x3) = FData4 x x1 x2 x3
--   {-# INLINE consF0 #-}
-- instance ConsFData0 FData '[x1, x2, x3, x4, x5] where
--   consF0 x (FData4 x1 x2 x3 x4) = FData5 x x1 x2 x3 x4
--   {-# INLINE consF0 #-}
-- 
-- instance ConsFData1 FData '[] where
--   consF1 x (FData0) = FData1 x
--   {-# INLINE consF1 #-}
-- instance ConsFData1 FData '[x1] where
--   consF1 x (FData1 x1) = FData2 x x1
--   {-# INLINE consF1 #-}
-- instance ConsFData1 FData '[x1, x2] where
--   consF1 x (FData2 x1 x2) = FData3 x x1 x2
--   {-# INLINE consF1 #-}
-- instance ConsFData1 FData '[x1, x2, x3] where
--   consF1 x (FData3 x1 x2 x3) = FData4 x x1 x2 x3
--   {-# INLINE consF1 #-}
-- instance ConsFData1 FData '[x1, x2, x3, x4] where
--   consF1 x (FData4 x1 x2 x3 x4) = FData5 x x1 x2 x3 x4
--   {-# INLINE consF1 #-}
--
-- instance RemoveElem FData '[x1] where
--   removeElem SFirstIndexZero (FData1 _x1) = FData0
--   {-# INLINE removeElem #-}
-- 
--   unRemoveElem SFirstIndexZero x (FData0) = FData1 x
--   {-# INLINE unRemoveElem #-}
-- 
-- instance RemoveElem FData '[x1, x2] where
--   removeElem SFirstIndexZero (FData2 _x1 x2)                        = FData1 x2
--   removeElem (SFirstIndexSucc Refl SFirstIndexZero) (FData2 x1 _x2) = FData1 x1
--   {-# INLINE removeElem #-}
-- 
--   unRemoveElem SFirstIndexZero x (FData1 x2)                        = FData2 x x2
--   unRemoveElem (SFirstIndexSucc Refl SFirstIndexZero) x (FData1 x1) = FData2 x1 x
--   {-# INLINE unRemoveElem #-}
-- 
-- instance RemoveElem FData '[x1, x2, x3] where
--   removeElem SFirstIndexZero (FData3 _x1 x2 x3)                                               = FData2 x2 x3
--   removeElem (SFirstIndexSucc Refl SFirstIndexZero) (FData3 x1 _x2 x3)                        = FData2 x1 x3
--   removeElem (SFirstIndexSucc Refl (SFirstIndexSucc Refl SFirstIndexZero)) (FData3 x1 x2 _x3) = FData2 x1 x2
--   {-# INLINE removeElem #-}
-- 
--   unRemoveElem SFirstIndexZero x (FData2 x2 x3)                                               = FData3 x x2 x3
--   unRemoveElem (SFirstIndexSucc Refl SFirstIndexZero) x (FData2 x1 x3)                        = FData3 x1 x x3
--   unRemoveElem (SFirstIndexSucc Refl (SFirstIndexSucc Refl SFirstIndexZero)) x (FData2 x1 x2) = FData3 x1 x2 x
--   {-# INLINE unRemoveElem #-}
