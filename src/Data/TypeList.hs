{-# LANGUAGE UndecidableSuperClasses, DefaultSignatures, AllowAmbiguousTypes, UndecidableInstances, DataKinds, TypeFamilies #-}
-- | This module provides utilities for working with type-level lists in Haskell.
-- It defines various types and functions to manipulate type-level lists, including
-- finite lists, constrained lists, and dynamic lists.
--
-- This module is considered INTERNAL, you can use it but be aware that the API may change without major version bumps.
module Data.TypeList
  ( module Data.TypeList.FList
  , module Data.TypeList.UList
  , module Data.TypeList.Families
  , module Data.TypeList.ConsFData
  , module Data.TypeList
  , module Data.Result
  ) where

import Data.TypeList.Families
import Data.TypeList.FList
import Data.TypeList.UList
import Data.TypeList.ConsFData
import Data.TypeList.ConsFData.Pattern
import Data.Type.Equality
import Data.Result
import Data.Kind
import Unsafe.Coerce

-- | A class carrying the proof of the existence of an element in a list.
class InList (e :: Type) (es :: [Type]) where
  singIndex      :: SNat (FirstIndex e es)
  singFirstIndex :: SFirstIndex e es
  proofIndex     :: AtIndex es (FirstIndex e es) :~: e
  elemIndex      :: Elem e es

  getEMaybe :: EList es -> Maybe e
  getEMaybe = getEMaybeS (singFirstIndex @e @es)
  {-# INLINE getEMaybe #-}

  embedE :: e -> EList es
  embedE = embedES (singFirstIndex @e @es)
  {-# INLINE embedE #-}

class InList e es => In' flist e es where
  getIn :: flist f es -> f e
  default getIn :: (ConsFData flist) => flist f es -> f e
  getIn = getInS (singFirstIndex @e @es)
  {-# INLINE getIn #-}

  modifyIn :: (f e -> f e) -> flist f es -> flist f es
  default modifyIn :: (ConsFData flist) => (f e -> f e) -> flist f es -> flist f es
  modifyIn f = modifyInS (singFirstIndex @e @es) f
  {-# INLINE modifyIn #-}

-- | Axiom
firstIndexTraverseNotEqElem :: forall e t ts. (NotEq e t, InList e ts) => FirstIndex e (t : ts) :~: Succ (FirstIndex e ts)
firstIndexTraverseNotEqElem = unsafeCoerce Refl
{-# INLINE firstIndexTraverseNotEqElem #-}

-- | Axiom
axiomInImpliesNonEmpty :: InList e ts => NonEmpty ts :~: True
axiomInImpliesNonEmpty = unsafeCoerce Refl
{-# INLINE axiomInImpliesNonEmpty #-}

-- | Base case for the InList class. NotIn e ts => 
instance InList e (e : ts) where
  singIndex = SZero
  {-# INLINE singIndex #-}
  singFirstIndex = SFirstIndexZero
  {-# INLINE singFirstIndex #-}
  proofIndex = Refl
  {-# INLINE proofIndex #-}
  elemIndex = EZ
  {-# INLINE elemIndex #-}
  embedE = EHead
  {-# INLINE embedE #-}
  getEMaybe = \case
    EHead x  -> Just x
    ETail _  -> Nothing
  {-# INLINE getEMaybe #-}

instance In' FList e (e : ts) where
  getIn = \(e :** _) -> e
  {-# INLINE getIn #-}
  modifyIn f = \(x :** xs) -> f x :** xs
  {-# INLINE modifyIn #-}
  
-- | InListductive case for the In class. UniqueIn e (t : ts), 
instance {-# OVERLAPPABLE #-} (NotEq e t, InList e ts) => InList e (t : ts) where
  singIndex = case firstIndexTraverseNotEqElem @e @t @ts of
    Refl -> SSucc (singIndex @e @ts)
  {-# INLINE singIndex #-}
  singFirstIndex = case firstIndexTraverseNotEqElem @e @t @ts of
    Refl -> case axiomInImpliesNonEmpty @e @ts of
      Refl -> SFirstIndexSucc Refl (singFirstIndex @e @ts)
  {-# INLINE singFirstIndex #-}
  proofIndex = case firstIndexTraverseNotEqElem @e @t @ts of
    Refl -> case proofIndex @e @ts of Refl -> Refl
  {-# INLINE proofIndex #-}
  elemIndex = ES elemIndex
  {-# INLINE elemIndex #-}
  embedE = ETail . embedE
  {-# INLINE embedE #-}
  getEMaybe = \case
    EHead _  -> Nothing
    ETail es -> getEMaybe es
  {-# INLINE getEMaybe #-}

instance {-# OVERLAPPABLE #-} (flist ~ FList, NonEmpty ts ~ True, FDataConstraint flist e (t : ts), ConsFDataList flist (t:ts), NotEq e t, In' flist e ts) => In' FList e (t : ts) where
  getIn = \(_ :*** xs) -> getIn xs
  {-# INLINE getIn #-}
  modifyIn f = \(x :*** xs) -> x :*** modifyIn f xs
  {-# INLINE modifyIn #-}

instance {-# INCOHERENT #-} InList e es => In' FList e es

class SubList (flist :: (Type -> Type) -> [Type] -> Type) (ys :: [Type]) (xs :: [Type]) where
  getSubListF    ::  flist f xs -> flist f ys -- ^ Get the sublist from the FList.
  subListModifyF :: (flist f ys -> flist f ys) -> flist f xs -> flist f xs -- ^ Modify the sublist in the FList.

class SubListEmbed (ys :: [Type]) (xs :: [Type]) where
  subListResultEmbed :: Result ys a -> Result xs a -- ^ Embed the result of a sublist operation.

subListUpdateF :: (SubList flist ys xs) => flist f xs -> flist f ys -> flist f xs
subListUpdateF xs ys = subListModifyF (const ys) xs
{-# INLINE subListUpdateF #-}

instance ConsFNil c => SubList c '[] xs where
  getSubListF _ = fNil
  {-# INLINE getSubListF #-}
  subListModifyF _ xs = xs
  {-# INLINE subListModifyF #-}

instance SubListEmbed '[] xs where
  subListResultEmbed (RSuccess a) = RSuccess a
  {-# INLINE subListResultEmbed #-}

instance (InList y xs, SubListEmbed ys xs) => SubListEmbed (y:ys) xs where
  subListResultEmbed (RSuccess a)          = RSuccess a
  subListResultEmbed (RFailure (EHead y))  = RFailure (embedE y)
  subListResultEmbed (RFailure (ETail ys)) = subListResultEmbed (RFailure ys)
  {-# INLINE subListResultEmbed #-}

-- | Induction case for the SubList class.
instance (In' c y xs, ConsFDataList c (y:ys), SubList c ys xs) => SubList c (y : ys) xs where
  getSubListF xs = consF0 (getIn xs) (getSubListF xs)
  {-# INLINE getSubListF #-}
  subListModifyF f xs =
    let hy :*** hys = f (getSubListF xs)
    in modifyIn (const hy) $ subListUpdateF xs hys
  {-# INLINE subListModifyF #-}

instance {-# INCOHERENT #-} ConsFDataList c (x:xs) => SubList c xs (x:xs) where
  getSubListF (_ :*** xs) = xs
  {-# INLINE getSubListF #-}
  subListModifyF f (x :*** xs) = x :*** f xs
  {-# INLINE subListModifyF #-}

instance {-# INCOHERENT #-} SubList c xs xs where
  getSubListF = id
  {-# INLINE getSubListF #-}
  subListModifyF = id
  {-# INLINE subListModifyF #-}

instance {-# INCOHERENT #-} SubListEmbed xs xs where
  subListResultEmbed = id
  {-# INLINE subListResultEmbed #-}
