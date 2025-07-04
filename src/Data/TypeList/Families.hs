{-# LANGUAGE UndecidableSuperClasses, PatternSynonyms, ViewPatterns, AllowAmbiguousTypes, UndecidableInstances, DataKinds, TypeOperators, LinearTypes, TypeFamilies, GADTs, PolyKinds, ScopedTypeVariables, ImpredicativeTypes #-}
module Data.TypeList.Families where

import Data.Kind (Type, Constraint)
import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(..))
import Fusion.Plugin.Types (Fuse(..))
import GHC.TypeError

data Nat where
  Zero :: Nat
  Succ :: Nat -> Nat

{-# ANN type SNat Fuse #-}
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

-- | ts - sub = rs
data Subtract (ts :: [Type]) (sub :: [Type]) (rs :: [Type]) where
  SubNil   :: Subtract ts '[] ts
  SubAddAC :: Proxy t -> Subtract as bs cs -> Subtract (t : as) bs (t : cs)

-- | A data-level proof of the existence of a sublist in a list.
--
-- Examples
--
-- sub12 :: Sub '[y1, y2] (y : y1 : y2 : ys)
-- sub12 = e1 `SS` e2 `SS` SZ
--
-- look at this type signature, how cool is that? owo it claims that the latter list contains [y1, y2] in the exact position we require
data SSub (ys :: [Type]) (xs :: [Type]) where
  SubZ :: SSub '[] xs                              -- ^ Base case: the empty sublist.
  SubS :: Elem y xs -> SSub ys xs -> SSub (y:ys) xs -- ^ Inductive case: the head element is in the list.

-- | singleton GADT for class
-- data SSubList (ys :: [Type]) (xs :: [Type]) where
--   SSubNil      :: SSubList '[] xs
--   SSubListCons
--     :: SFirstIndex y xs
--     -> SSubList ys xs
--     -> SSubList (y : ys) xs

-- | A type-level function that concatenates two type-level lists.
type family (xs :: [Type]) ++ (ys :: [Type]) :: [Type] where
  '[] ++ bs = bs
  (a:as) ++ bs = a : (as ++ bs)

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

{-# ANN type SFirstIndex Fuse #-}
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

data Sing t = Sing

data SingList (ts :: [Type]) where
  SNil  :: SingList '[]
  SCons :: Sing t -> SingList ts -> SingList (t : ts)

class ListSing (ts :: [Type]) where
  singList :: SingList ts

instance ListSing '[] where
  singList = SNil
  {-# INLINE singList #-}

instance ListSing ts => ListSing (t : ts) where
  singList = SCons Sing singList
  {-# INLINE singList #-}

type family FDataConstraint (flist :: (Type -> Type) -> [Type] -> Type) (e :: Type) (es :: [Type]) :: Constraint
