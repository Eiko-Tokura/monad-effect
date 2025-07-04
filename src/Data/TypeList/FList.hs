{-# LANGUAGE UndecidableInstances #-}
module Data.TypeList.FList where

import Data.Coerce
import Data.Kind (Constraint, Type)
import Data.Type.Equality
import Data.TypeList.ConsFData
import Data.TypeList.Families

-- | A type-level list applied to a type-level function, representing a product.
-- It has a strict head and tail.
-- the ! bang pattern here is to make it strict because it might cause trouble when putting in a stateful monad. Alternatively we can also write a strict version FList, SFList.
data FList (f :: Type -> Type) (ts :: [Type]) where
  FNil  :: FList f '[]
  FCons :: !(f t) -> !(FList f ts) -> FList f (t : ts)
infixr 5 `FCons`

newtype HBox a = HBox { unHBox :: a }

type HList = FList HBox

liftHBox :: (a -> b) -> HBox a -> HBox b
liftHBox = coerce
{-# INLINE liftHBox #-}

-- CList also becomes an example of FList
data CBox (constraint :: Type -> Constraint) (t :: Type) where
  CBox :: constraint t => !t -> CBox constraint t

type CList constraint = FList (CBox constraint)

type instance FDataConstraint FList e ts = ()

instance ConsFNil FList where
  fNil = FNil
  {-# INLINE fNil #-}

instance ConsFData FList where
  unConsF (x `FCons` xs) = (x, xs)
  {-# INLINE unConsF #-}

  consF x xs = x `FCons` xs
  {-# INLINE consF #-}

instance WhenNonEmpty ts (UnConsFData FList ts) => UnConsFData FList (t:ts) where
  unConsFData (x `FCons` xs) = (x, xs)
  {-# INLINE unConsFData #-}

instance WhenNonEmpty ts (ConsFData0 FList ts) => ConsFData0 FList (t:ts) where
  consF0 x xs = x `FCons` xs
  {-# INLINE consF0 #-}

instance When (NonEmpty ts) (ConsFData1 FList (Tail ts)) => ConsFData1 FList ts where
  consF1 x xs = x `FCons` xs
  {-# INLINE consF1 #-}

instance
  ( flist ~ FList
  , WhenNonEmpty ts (ConsFDataList flist (Tail ts))
  , ConsFNil flist
  , WhenNonEmpty ts (UnConsFData flist ts)
  , WhenNonEmpty ts (ConsFData0 flist ts)
  , ConsFData1 flist ts
  ) => ConsFDataList FList ts 

instance WhenNonEmpty ts (RemoveElem FList ts) => RemoveElem FList (t : ts) where
  removeElem SFirstIndexZero (_ `FCons` xs) = xs
  removeElem (SFirstIndexSucc Refl n) (x `FCons` xs) = x `FCons` removeElem n xs
  {-# INLINE removeElem #-}

  unRemoveElem SFirstIndexZero x xs = x `FCons` xs
  unRemoveElem (SFirstIndexSucc Refl n) x (y `FCons` ys) = y `FCons` unRemoveElem n x ys
  {-# INLINE unRemoveElem #-}
