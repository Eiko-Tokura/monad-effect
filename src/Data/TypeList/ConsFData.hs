{-# LANGUAGE PatternSynonyms, ViewPatterns, UndecidableSuperClasses #-}
-- | This module is internal
module Data.TypeList.ConsFData where

import Data.TypeList.Families
import Data.Type.Equality
import Data.Kind (Type)
import Data.Proxy

class ConsFNil (flist :: (Type -> Type) -> [Type] -> Type) where
  fNil  :: flist f '[]

class When (NonEmpty (Tail ts)) (UnConsFData flist (Tail ts)) => UnConsFData flist ts where
  unConsFData :: flist f ts -> (f (Head ts), flist f (Tail ts))

class When (NonEmpty ts) (ConsFData1 flist (Tail ts)) => ConsFData1 flist ts where
  consF1 :: f t -> flist f ts -> flist f (t : ts)

class When (NonEmpty (Tail ts)) (ConsFData0 flist (Tail ts)) => ConsFData0 flist ts where
  consF0 :: f (Head ts) -> flist f (Tail ts) -> flist f ts

class When (NonEmpty (Tail ts)) (RemoveElem flist (Tail ts)) => RemoveElem flist (ts :: [Type]) where
  removeElem :: SFirstIndex t ts -> flist f ts -> flist f (Remove (FirstIndex t ts) ts)

  unRemoveElem :: SFirstIndex t ts -> f t -> flist f (Remove (FirstIndex t ts) ts) -> flist f ts

class When (NonEmpty (Tail ts)) (ReplaceElem flist (Tail ts)) => ReplaceElem flist (ts :: [Type]) where
  replaceElem :: SFirstIndex t ts -> f t' -> flist f ts -> flist f (Replace (FirstIndex t ts) t' ts)

  unReplaceElem :: SFirstIndex t ts -> Proxy t' -> (f t' -> f t) -> flist f (Replace (FirstIndex t ts) t' ts) -> flist f ts

class ConsFNil flist => ConsFData flist where
  unConsF :: flist f (t : ts) -> (f t, flist f ts)

  consF   :: f t -> flist f ts -> flist f (t : ts)

  getInS :: SFirstIndex t ts -> flist f ts -> f t
  getInS SFirstIndexZero          (unConsF -> (x, _)) = x
  getInS (SFirstIndexSucc Refl n) (unConsF -> (_, xs)) = getInS n xs
  {-# INLINE getInS #-}
 
  modifyInS :: SFirstIndex t ts -> (f t -> f t) -> flist f ts -> flist f ts
  modifyInS SFirstIndexZero          f (unConsF -> (x, xs)) = f x `consF` xs
  modifyInS (SFirstIndexSucc Refl n) f (unConsF -> (x, xs)) = x `consF` modifyInS n f xs
  {-# INLINE modifyInS #-}

class
  ( WhenNonEmpty ts (ConsFDataList flist (Tail ts))
  , ConsFNil flist
  , WhenNonEmpty ts (UnConsFData flist ts)
  , WhenNonEmpty ts (ConsFData0 flist ts)
  , ConsFData1 flist ts
  )
  => ConsFDataList flist ts

-- | Get an element from a finite list using an element proof.
getE :: ConsFData flist => Elem e l -> flist f l -> f e
getE  EZ    = \(unConsF -> (x, _))  -> x
getE (ES n) = \(unConsF -> (_, xs)) -> getE n xs

putE :: ConsFData flist => Elem e l -> f e -> flist f l -> flist f l
putE EZ     x' (unConsF -> (_, xs)) = x' `consF` xs
putE (ES n) y' (unConsF -> (x, xs)) = x  `consF` putE n y' xs
