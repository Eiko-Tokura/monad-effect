{-# LANGUAGE UndecidableSuperClasses, PatternSynonyms, ViewPatterns, AllowAmbiguousTypes, UndecidableInstances, DataKinds, TypeOperators, LinearTypes, TypeFamilies, GADTs, PolyKinds, ScopedTypeVariables, ImpredicativeTypes #-}
-- | This module provides the fundational types and type families
module Data.TypeList.Families where

import Data.Kind (Type, Constraint)
import Data.Type.Equality ((:~:)(..))
import GHC.TypeError
import Unsafe.Coerce (unsafeCoerce)

data Nat where
  Zero :: Nat
  Succ :: Nat -> Nat

data SNat (n :: Nat) where
  SZero :: SNat 'Zero
  SSucc :: SNat n -> SNat ('Succ n)

type family Head (ts :: [Type]) :: Type where
  Head (t ': ts) = t
  Head '[] = TypeError ('Text "Head: Empty list")

type family Tail (ts :: [Type]) :: [Type] where
  Tail (t ': ts) = ts
  Tail '[] = TypeError ('Text "Tail: Empty list")

-- | Proof of the existence of an element in a list.
data Elem e l where
  EZ :: Elem x (x : xs)              -- ^ Base case: the element is the head of the list.
  ES :: Elem x ys -> Elem x (y : ys) -- ^ Inductive case: the element is in the tail of the list.

-- | A type-level function that concatenates two type-level lists.
type family (xs :: [Type]) ++ (ys :: [Type]) :: [Type] where
  '[] ++ bs = bs
  (a:as) ++ bs = a : (as ++ bs)

type family IfBool (b :: Bool) (t :: k) (e :: k) where
  IfBool 'True  a _ = a
  IfBool 'False _ b = b

type family When (b :: Bool) (c :: Constraint) :: Constraint where
  When bool c = IfBool bool c (() :: Constraint)

type WhenNonEmpty ts c = When (NonEmpty ts) c

type family AddIfNotElem (a :: Type) (bs :: [Type]) :: [Type] where
  AddIfNotElem a bs = IfBool (ElemIn a bs) bs (a ': bs)

class CheckIfElem (a :: Type) (bs :: [Type]) where
  singIfElem :: Either (ElemIn a bs :~: False) (ElemIn a bs :~: True, SFirstIndex a bs)

instance CheckIfElem a '[] where
  singIfElem = Left Refl
  {-# INLINE singIfElem #-}

instance CheckIfElem a (a ': xs) where
  singIfElem = Right (Refl, SFirstIndexZero)
  {-# INLINE singIfElem #-}

-- | Axiom, validity relies only on the instance resolution
axiomNotEqElem :: (NotEq a b) => ElemIn a bs :~: bool -> ElemIn a (b : bs) :~: bool
axiomNotEqElem _ = unsafeCoerce Refl
{-# INLINE axiomNotEqElem #-}

-- | Axiom, validity relies only on the instance resolution
firstIndexTraverseNotEqElemIn :: forall e t ts. (NotEq e t, ElemIn e ts ~ True) => FirstIndex e (t : ts) :~: Succ (FirstIndex e ts)
firstIndexTraverseNotEqElemIn = unsafeCoerce Refl :: FirstIndex e (t : ts) :~: Succ (FirstIndex e ts)
{-# INLINE firstIndexTraverseNotEqElemIn #-}

instance {-# OVERLAPPABLE #-} (NotEq a b, CheckIfElem a bs) => CheckIfElem a (b ': bs) where
  singIfElem = case singIfElem @a @bs of
    Left r@Refl -> case axiomNotEqElem @a @b @bs r of Refl -> Left Refl

    Right (r@Refl, SFirstIndexZero) -> case axiomNotEqElem @a @b @bs r of
      Refl -> case firstIndexTraverseNotEqElemIn @a @b @bs of
        Refl -> Right (Refl, SFirstIndexSucc Refl SFirstIndexZero)

    Right (r@Refl, SFirstIndexSucc Refl i) -> case axiomNotEqElem @a @b @bs r of
      Refl -> case firstIndexTraverseNotEqElemIn @a @b @bs of
        Refl -> Right (Refl, SFirstIndexSucc Refl (SFirstIndexSucc Refl i))
  {-# INLINE singIfElem #-}

type family ElemIn (a :: Type) (bs :: [Type]) :: Bool where
  ElemIn a '[]       = 'False
  ElemIn a (a ': _)  = 'True
  ElemIn a (_ ': xs) = ElemIn a xs

type family NotEq (a :: Type) (b :: Type) :: Constraint where
  NotEq a a = TypeError ('Text "Type " ':<>: 'ShowType a ':<>: 'Text " duplicated")
  NotEq a b = ()

type family NotIn (e :: Type) (ts :: [Type]) :: Constraint where
  NotIn e '[] = ()
  NotIn e (e ': ts) = TypeError ('Text "Type " ':<>: 'ShowType e ':<>: 'Text " is already in the list")
  NotIn e (t ': ts) = NotIn e ts

type family UniqueIn (e :: Type) (ts :: [Type]) :: Constraint where
  UniqueIn e '[] = ()
  UniqueIn e (e ': ts) = NotIn e ts
  UniqueIn e (t ': ts) = UniqueIn e ts

type family UniqueList (ts :: [Type]) :: Constraint where
  UniqueList '[] = ()
  UniqueList (t ': ts) = (NotIn t ts, UniqueList ts)

type family NonEmpty (ts :: [Type]) :: Bool where
  NonEmpty '[] = 'False
  NonEmpty (t ': ts) = 'True

type family Remove (e :: Nat) (ts :: [Type]) :: [Type] where
  Remove e '[]              = '[]
  Remove Zero     (t ': ts) = ts
  Remove (Succ n) (t ': ts) = t : Remove n ts

type family AtIndex (ts :: [Type]) (n :: Nat) :: Type where
  AtIndex (t ': ts) 'Zero = t
  AtIndex (t ': ts) ('Succ n) = AtIndex ts n
  AtIndex '[] _ = TypeError ('Text "AtIndex: Index out of bounds")

-- | Calculate the first index
type family FirstIndex (e :: Type) (ts :: [Type]) :: Nat where
  FirstIndex e (e ': ts) = 'Zero
  FirstIndex e (t ': ts) = 'Succ (FirstIndex e ts)
  FirstIndex e '[] = TypeError ('Text "Error evaluating Index: Type " ':<>: 'ShowType e ':<>: 'Text " is not in the list")

-- | due to unsaturated type family not implemented yet, please note that arr can only be data / data family
type family FmapMaybe (arr :: a -> b) (ma :: Maybe a) :: Maybe b where 
  FmapMaybe _   'Nothing  = 'Nothing
  FmapMaybe arr ('Just x) = 'Just (arr x)

type family FirstIndexMaybe (e :: Type) (ts :: [Type]) :: Maybe Nat where
  FirstIndexMaybe e (e ': ts) = 'Just 'Zero
  FirstIndexMaybe e (t ': ts) = FmapMaybe 'Succ (FirstIndexMaybe e ts)
  FirstIndexMaybe e '[]       = 'Nothing

data SFirstIndex (e' :: Type) (ts' :: [Type]) where
  SFirstIndexZero :: SFirstIndex e (e ': ts) -- ^ Base case: the element is the head of the list.
  SFirstIndexSucc
    :: NonEmpty ts ~ 'True
    => !(FirstIndex e (t ': ts) :~: Succ (FirstIndex e ts)) -- ^ Takes a proof that the first index of e in (t ': ts) is one more than in ts.
    -> !(SFirstIndex e ts)
    -> SFirstIndex e (t ': ts) -- ^ Inductive case: the element is in the tail of the list.

singFirstIndexToSNat :: SFirstIndex e ts -> SNat (FirstIndex e ts)
singFirstIndexToSNat SFirstIndexZero          = SZero
singFirstIndexToSNat (SFirstIndexSucc Refl s) = SSucc (singFirstIndexToSNat s)
{-# INLINE singFirstIndexToSNat #-}

type family FDataConstraint (flist :: (Type -> Type) -> [Type] -> Type) (e :: Type) (es :: [Type]) :: Constraint
