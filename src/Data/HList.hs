{-# LANGUAGE UndecidableSuperClasses, AllowAmbiguousTypes, UndecidableInstances, DataKinds, TypeOperators, LinearTypes, TypeFamilies, GADTs, PolyKinds, ScopedTypeVariables, ImpredicativeTypes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
-- | This module provides utilities for working with type-level lists in Haskell.
-- It defines various types and functions to manipulate type-level lists, including
-- finite lists, constrained lists, and dynamic lists.
module Data.HList where

import Data.Kind
import Data.Proxy
import Data.Default
import Data.Type.Equality ((:~:)(..))
import GHC.TypeError
import Unsafe.Coerce (unsafeCoerce)

-- | A type-level list applied to a type-level function, representing a product.
-- It has a strict head and tail.
-- the ! bang pattern here is to make it strict because it might cause trouble when putting in a stateful monad. Alternatively we can also write a strict version FList, SFList.
data FList (f :: Type -> Type) (ts :: [Type]) where
  FNil  :: FList f '[]
  (:**) :: !(f t) -> !(FList f ts) -> FList f (t : ts)
infixr 5 :**

-- We may try something like this in the future, use Template Haskell to generate all the instances for FData
--
-- this will enable fast access and update, improving performance
--
-- data FData (f :: Type -> Type) (ts :: [Type]) where
--   FData0 :: FData f '[]
--   FData1 :: { fdata1_0 :: !(f t) } -> FData f '[t]
--   FData2 :: { fdata2_0 :: !(f t1), fdata2_1 :: !(f t2) } -> FData f '[t1, t2]
--   FData3 :: { fdata3_0 :: !(f t1), fdata3_1 :: !(f t2), fdata3_2 :: !(f t3) } -> FData f '[t1, t2, t3]
--   FData4 :: { fdata4_0 :: !(f t1), fdata4_1 :: !(f t2), fdata4_2 :: !(f t3), fdata4_3 :: !(f t4) } -> FData f '[t1, t2, t3, t4]
--   FData5 :: { fdata5_0 :: !(f t1), fdata5_1 :: !(f t2), fdata5_2 :: !(f t3), fdata5_3 :: !(f t4), fdata5_4 :: !(f t5) } -> FData f '[t1, t2, t3, t4, t5]

-- | A type-level list representing a simple sum of types.
data SList (ts :: [Type]) where
  SEmpty :: SList ts
  SHead  :: t -> SList (t : ts)
  STail  :: SList ts -> SList (t : ts)

-- | Sum of types, but non-empty.
-- You cannot construct EList '[]
data EList (ts :: [Type]) where
  EHead :: !t -> EList (t : ts)
  ETail :: !(EList ts) -> EList (t : ts)

-- | Error type that adapt to the type-level list of errors.
data Result (es :: [Type]) (a :: Type) where
  RSuccess :: a -> Result es a
  RFailure :: !(EList es) -> Result es a

resultNoError :: Result '[] a -> a
resultNoError (RSuccess a) = a

resultToEither :: Result (e ': es) a -> Either (EList (e ': es)) a
resultToEither (RSuccess a) = Right a
resultToEither (RFailure es) = Left es

resultMapErrors :: (EList es -> EList es') -> Result es a -> Result es' a
resultMapErrors _ (RSuccess a) = RSuccess a
resultMapErrors f (RFailure es) = RFailure (f es)

fromElistSingleton :: EList '[s] -> s
fromElistSingleton (EHead x) = x

instance Functor (Result es) where
  fmap f (RSuccess a)  = RSuccess (f a)
  fmap _ (RFailure es) = RFailure es
  {-# INLINE fmap #-}

instance Applicative (Result es) where
  pure = RSuccess
  RSuccess f <*> RSuccess a = RSuccess (f a)
  RFailure es <*> _ = RFailure es
  _ <*> RFailure es = RFailure es
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

instance Monad (Result es) where
  RSuccess a >>= f = f a
  RFailure es >>= _ = RFailure es
  {-# INLINE (>>=) #-}

-- | A type-level list applied to a type-level function, representing a sum.
data UList (f :: Type -> Type) (ts :: [Type]) where
  UNil  :: UList f '[]
  UHead :: f t -> UList f (t : ts)
  UTail :: UList f ts -> UList f (t : ts)

-- | ts - sub = rs
data Subtract (ts :: [Type]) (sub :: [Type]) (rs :: [Type]) where
  SubNil   :: Subtract ts '[] ts
  SubAddAC :: t -> Subtract as bs cs -> Subtract (t : as) bs (t : cs)

---------------------------------- Constraint Lists ----------------------------------

-- | A type-level list with constraints, allowing for the extraction of elements
-- based on certain type-level conditions.
data CList (f :: Type -> Constraint) (ts :: [Type]) where
  CNil  :: CList f '[]
  CCons :: f t => t -> CList f ts -> CList f (t : ts)
infixr 5 `CCons`

-- | A dynamic type-level list with constraints, where no information about
-- the contents is available at the type level.
data CListDynamic (c :: Type -> Constraint) where
  CDNil  :: CListDynamic f
  CDCons :: c t => t -> CListDynamic c -> CListDynamic c
infixr 5 `CDCons`

-- | A constrained finite list that associates constraints with types.
data CFList (c :: k -> Constraint) (f :: k -> Type) (ts :: [k]) where
  CFNil  :: CFList c f '[]
  CFCons :: c t => f t -> CFList c f ts -> CFList c f (t : ts)
infixr 5 `CFCons`

-- | Default instance for an empty constrained finite list.
instance Default (CFList c f '[]) where
  def = CFNil
  {-# INLINE def #-}

-- | Default instance for a non-empty constrained finite list.
instance (c p, Default (CFList c Proxy ps)) => Default (CFList c Proxy (p : ps)) where
  def = CFCons Proxy def
  {-# INLINE def #-}

-- | Map a function over a constrained finite list.
cfListMap :: CFList c f ts -> (forall t. c t => f t -> a) -> [a]
cfListMap CFNil _ = []
cfListMap (CFCons t ts) f = f t : cfListMap ts f
{-# INLINE cfListMap #-}

-- | Pick an element from a constrained finite list that satisfies a predicate.
cfListPickElem :: CFList c f ts -> (forall t. c t => f t -> Bool) -> Maybe ((forall t. c t => f t -> a) -> a)
cfListPickElem CFNil _ = Nothing
cfListPickElem (CFCons t ts) predicate
  | predicate t = Just $ \f -> f t
  | otherwise   = cfListPickElem ts predicate
{-# INLINE cfListPickElem #-}

-- | Pick an element from a constrained list that satisfies a predicate.
cListPickElem :: CList c ts -> (forall t. c t => t -> Bool) -> Maybe ((forall t. c t => t -> a) -> a)
cListPickElem CNil _ = Nothing
cListPickElem (CCons t ts) predicate
  | predicate t = Just $ \f -> f t
  | otherwise   = cListPickElem ts predicate
{-# INLINE cListPickElem #-}

-- | Pick an element from a dynamic constrained list that satisfies a predicate.
cListDynamicPickElem :: CListDynamic c -> (forall t. c t => t -> Bool) -> Maybe ((forall t. c t => t -> a) -> a)
cListDynamicPickElem CDNil _ = Nothing
cListDynamicPickElem (CDCons t ts) predicate
  | predicate t = Just $ \f -> f t
  | otherwise   = cListDynamicPickElem ts predicate
{-# INLINE cListDynamicPickElem #-}

-- | Map a function over a constrained list.
cListMap :: CList c ts -> (forall t. c t => t -> a) -> [a]
cListMap CNil _ = []
cListMap (CCons t ts) f = f t : cListMap ts f
{-# INLINE cListMap #-}

-- | Map a function over a dynamic constrained list.
cListDynamicMap :: CListDynamic c -> (forall t. c t => t -> a) -> [a]
cListDynamicMap CDNil _ = []
cListDynamicMap (CDCons t ts) f = f t : cListDynamicMap ts f
{-# INLINE cListDynamicMap #-}

-- | A class for working with constraint lists, providing methods to use and pick constraints.
class ConstraintList (c :: k -> Constraint) (ts :: [k]) where
  useConstraint  :: Proxy c -> Proxy ts -> (forall t. c t => Proxy t -> a) -> [a]
  pickConstraint :: Proxy c -> Proxy ts -> (forall t. c t => Proxy t -> Bool) -> Maybe ((forall t. c t => Proxy t -> a) -> a)

-- | Instance for an empty constraint list.
instance ConstraintList c '[] where
  useConstraint  _ _ _ = []
  pickConstraint _ _ _ = Nothing
  {-# INLINE useConstraint #-}
  {-# INLINE pickConstraint #-}

-- | Instance for a non-empty constraint list.
instance (c t, ConstraintList c ts) => ConstraintList c (t : ts) where
  useConstraint  pc _ f = f (Proxy @t) : useConstraint pc (Proxy @ts) f
  pickConstraint pc _ predicate
    | predicate (Proxy :: Proxy t) = Just $ \f -> f (Proxy @t)
    | otherwise = pickConstraint pc (Proxy @ts) predicate
  {-# INLINE useConstraint #-}
  {-# INLINE pickConstraint #-}

------------------------------Classical HList----------------------------------

-- | A type-level list representing a product.
data HList ts where
  Nil  :: HList '[]
  (:*) :: t -> HList ts -> HList (t : ts)
infixr 5 :*

-- | Get an element from a finite list using an element proof.
getEF :: Elem e l -> FList f l -> f e
getEF  EZ    (x :** _)  = x
getEF (ES n) (_ :** xs) = getEF n xs
{-# INLINE getEF #-}

-- | Proof of the existence of an element in a list.
data Elem e l where
  EZ :: Elem x (x : xs)              -- ^ Base case: the element is the head of the list.
  ES :: Elem x ys -> Elem x (y : ys) -- ^ Inductive case: the element is in the tail of the list.

-- | Get the element in the HList using the proof.
(!:) :: HList l -> Elem e l -> e
(!:) = flip getE
{-# INLINE (!:) #-}
infixl 9 !:

-- | Get the element in the HList using the proof.
getE :: Elem e l -> HList l -> e
getE  EZ    (x :* _)  = x
getE (ES n) (_ :* xs) = getE n xs
{-# INLINE getE #-}

-- | Put an element into the HList at the position specified by the proof.
putE :: Elem e l -> e -> HList l -> HList l
putE  EZ    x (_ :* xs) = x :* xs
putE (ES n) x (y :* xs) = y :* putE n x xs
{-# INLINE putE #-}

-- | A type-level function that concatenates two type-level lists.
type family (xs :: [Type]) ++ (ys :: [Type]) :: [Type] where
  '[] ++ bs = bs
  (a:as) ++ bs = a : (as ++ bs)

-- | A data-level proof of the existence of a sublist in a list.
--
-- Examples
--
-- sub12 :: Sub '[y1, y2] (y : y1 : y2 : ys)
-- sub12 = e1 `SS` e2 `SS` SZ
--
-- look at this type signature, how cool is that? owo it claims that the latter list contains [y1, y2] in the exact position we require
data Sub (ys :: [Type]) (xs :: [Type]) where
  SZ :: Sub '[] xs                              -- ^ Base case: the empty sublist.
  SS :: Elem y xs -> Sub ys xs -> Sub (y:ys) xs -- ^ Inductive case: the head element is in the list.

-- | Get the sublist in the HList using the proof.
getSub :: Sub ys xs -> HList xs -> HList ys
getSub SZ _        = Nil
getSub (SS e t) xs = getE e xs :* getSub t xs
{-# INLINE getSub #-}

-- | A class carrying the proof of the existence of an element in a list. UniqueIn e ts => 
class In e (ts :: [Type]) where
  singIndex :: SNat (FirstIndex e ts)
  singFirstIndex :: SFirstIndex e ts
  proofIndex :: AtIndex ts (FirstIndex e ts) :~: e
  elemIndex :: Elem e ts
  getH :: HList ts -> e                               -- ^ Get the element from the HList.
  getF :: FList f ts -> f e                           -- ^ Get the element from the FList.
  getEMaybe :: EList ts -> Maybe e                    -- ^ Get the element from the EList, if it exists.
  modifyH :: (e -> e) -> HList ts -> HList ts         -- ^ Modify the element in the HList.
  modifyF :: (f e -> f e) -> FList f ts -> FList f ts -- ^ Modify the element in the FList.
  embedU :: f e -> UList f ts                         -- ^ Embed the element into a UList.
  embedS :: e -> SList ts                             -- ^ Embed the element into a SList.
  embedE :: e -> EList ts                             -- ^ Embed the element into an EList.

data Nat where
  Zero :: Nat
  Succ :: Nat -> Nat

data SNat (n :: Nat) where
  SZero :: SNat 'Zero
  SSucc :: SNat n -> SNat ('Succ n)

-- | Calculate the first index
type family FirstIndex (e :: Type) (ts :: [Type]) :: Nat where
  FirstIndex e (e ': ts) = 'Zero
  FirstIndex e (t ': ts) = 'Succ (FirstIndex e ts)
  FirstIndex e '[] = TypeError ('Text "Error evaluating Index: Type " ':<>: 'ShowType e ':<>: 'Text " is not in the list")

data SFirstIndex (e :: Type) (ts :: [Type]) where
  SFirstIndexZero :: SFirstIndex e (e ': ts) -- ^ Base case: the element is the head of the list.
  SFirstIndexSucc
    :: (FirstIndex e (t ': ts) :~: Succ (FirstIndex e ts)) -- ^ Takes a proof that the first index of e in (t ': ts) is one more than in ts.
    -> (SFirstIndex e ts)
    -> SFirstIndex e (t ': ts) -- ^ Inductive case: the element is in the tail of the list.

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

type family Remove (e :: Nat) (ts :: [Type]) :: [Type] where
  Remove e '[]              = '[]
  Remove Zero     (t ': ts) = ts
  Remove (Succ n) (t ': ts) = t : Remove n ts

type family AtIndex (ts :: [Type]) (n :: Nat) :: Type where
  AtIndex (t ': ts) 'Zero = t
  AtIndex (t ': ts) ('Succ n) = AtIndex ts n
  AtIndex '[] _ = TypeError ('Text "AtIndex: Index out of bounds")

removeElemS :: SFirstIndex e ts -> FList f ts -> FList f (Remove (FirstIndex e ts) ts)
removeElemS SFirstIndexZero     (_ :** xs) = xs
removeElemS (SFirstIndexSucc Refl n) (x :** xs) = x :** removeElemS n xs
{-# INLINE removeElemS #-}

unRemoveElemS :: SFirstIndex e ts -> f e -> FList f (Remove (FirstIndex e ts) ts) -> FList f ts
unRemoveElemS SFirstIndexZero x xs = x :** xs
unRemoveElemS (SFirstIndexSucc Refl n) x (y :** ys) = y :** unRemoveElemS n x ys
{-# INLINE unRemoveElemS #-}

removeElem :: SNat n -> FList f ts -> FList f (Remove n ts)
removeElem _ FNil               = FNil
removeElem SZero     (_ :** xs) = xs
removeElem (SSucc n) (x :** xs) = x :** removeElem n xs
{-# INLINE removeElem #-}

getRemoveElemE :: SNat n -> Result es a -> Either (AtIndex es n) (Result (Remove n es) a)
getRemoveElemE _         (RSuccess a)          = Right (RSuccess a)
getRemoveElemE SZero     (RFailure (EHead e))  = Left e
getRemoveElemE SZero     (RFailure (ETail es)) = Right (RFailure es)
getRemoveElemE (SSucc _) (RFailure (EHead e))  = Right $ RFailure $ EHead e
getRemoveElemE (SSucc n) (RFailure (ETail es)) =
  case getRemoveElemE n (RFailure es) of
    Left  e              -> Left e
    Right (RFailure es') -> Right (RFailure $ ETail es')
    Right (RSuccess a)   -> Right (RSuccess a)
{-# INLINE getRemoveElemE #-}

-- | Note: this function builds on a proof that relies on a coercion,
-- it relies on the fact that the instance generation is done in the correct order.
-- if you change the order of instance generation, it will break.
getRemoveIn :: forall e ts f. (e `In` ts) => FList f ts -> (f e, FList f (Remove (FirstIndex e ts) ts))
getRemoveIn xs = (getF xs, removeElem (singIndex @e @ts) xs)
{-# INLINE getRemoveIn #-}

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

-- | Base case for the In class. NotIn e ts => 
instance In e (e : ts) where
  singIndex = SZero
  {-# INLINE singIndex #-}
  singFirstIndex = SFirstIndexZero
  {-# INLINE singFirstIndex #-}
  proofIndex = Refl
  {-# INLINE proofIndex #-}
  elemIndex = EZ
  {-# INLINE elemIndex #-}
  getH h = h !: EZ
  {-# INLINE getH #-}
  getF = getEF EZ
  {-# INLINE getF #-}
  getEMaybe (EHead x) = Just x
  getEMaybe (ETail _) = Nothing
  {-# INLINE getEMaybe #-}
  modifyH f (x :* xs) = f x :* xs
  {-# INLINE modifyH #-}
  modifyF g (x :** xs) = g x :** xs
  {-# INLINE modifyF #-}
  embedU = UHead
  {-# INLINE embedU #-}
  embedS = SHead
  {-# INLINE embedS #-}
  embedE = EHead
  {-# INLINE embedE #-}

type family NotEq (a :: Type) (b :: Type) :: Constraint where
  NotEq a a = TypeError ('Text "Type " ':<>: 'ShowType a ':<>: 'Text " duplicated")
  NotEq a b = ()

-- | Treated as an axiom
firstIndexTraverseNotEqElem :: forall e t ts. (NotEq e t, In e ts) => FirstIndex e (t : ts) :~: Succ (FirstIndex e ts)
firstIndexTraverseNotEqElem = case unsafeCoerce Refl :: FirstIndex e (t : ts) :~: Succ (FirstIndex e ts) of
  Refl -> Refl
{-# INLINE firstIndexTraverseNotEqElem #-}

-- | Inductive case for the In class. UniqueIn e (t : ts), 
instance {-# OVERLAPPABLE #-} (NotEq e t, In e ts) => In e (t : ts) where
  singIndex = case firstIndexTraverseNotEqElem @e @t @ts of
    Refl -> SSucc (singIndex @e @ts)
  {-# INLINE singIndex #-}
  singFirstIndex = case firstIndexTraverseNotEqElem @e @t @ts of
    Refl -> SFirstIndexSucc Refl (singFirstIndex @e @ts)
  {-# INLINE singFirstIndex #-}
  proofIndex = case firstIndexTraverseNotEqElem @e @t @ts of
    Refl -> case proofIndex @e @ts of Refl -> Refl
  {-# INLINE proofIndex #-}
  elemIndex = ES elemIndex
  {-# INLINE elemIndex #-}
  getH (_ :* xs) = getH xs
  {-# INLINE getH #-}
  getF (_ :** xs) = getF xs
  {-# INLINE getF #-}
  getEMaybe (EHead _)  = Nothing -- according to the constraint, we can prove that e /~ t
  getEMaybe (ETail xs) = getEMaybe xs
  {-# INLINE getEMaybe #-}
  modifyH f (x :* xs) = x :* modifyH f xs
  {-# INLINE modifyH #-}
  modifyF g (x :** xs) = x :** modifyF g xs
  {-# INLINE modifyF #-}
  embedU x = UTail $ embedU x
  {-# INLINE embedU #-}
  embedS x = STail $ embedS x
  {-# INLINE embedS #-}
  embedE x = ETail $ embedE x
  {-# INLINE embedE #-}

-- proofSubListXsXs :: SingList xs -> ProofSubList xs xs
-- proofSubListXsXs SNil = PSubNil
-- proofSubListXsXs (SCons t ts) = PSubAdd EZ _
-- {-# INLINE proofSubListXsXs #-}

proofGetH :: Elem e ts -> HList ts -> e
proofGetH EZ    (x :* _)  = x
proofGetH (ES n) (_ :* xs) = proofGetH n xs
{-# INLINE proofGetH #-}

proofGetF :: Elem e ts -> FList f ts -> f e
proofGetF EZ    (x :** _)  = x
proofGetF (ES n) (_ :** xs) = proofGetF n xs
{-# INLINE proofGetF #-}

proofGetEMaybe :: Elem e ts -> EList ts -> Maybe e
proofGetEMaybe EZ    (EHead x)   = Just x
proofGetEMaybe (ES n) (ETail xs) = proofGetEMaybe n xs
proofGetEMaybe _ _ = Nothing
{-# INLINE proofGetEMaybe #-}

proofModifyH :: Elem e ts -> (e -> e) -> HList ts -> HList ts
proofModifyH EZ    f (x :* xs) = f x :* xs
proofModifyH (ES n) f (y :* xs) = y :* proofModifyH n f xs
{-# INLINE proofModifyH #-}

proofModifyF :: Elem e ts -> (f e -> f e) -> FList f ts -> FList f ts
proofModifyF EZ    f (x :** xs) = f x :** xs
proofModifyF (ES n) f (y :** xs) = y :** proofModifyF n f xs
{-# INLINE proofModifyF #-}

proofEmbedU :: Elem e ts -> f e -> UList f ts
proofEmbedU EZ    x = UHead x
proofEmbedU (ES n) x = UTail $ proofEmbedU n x
{-# INLINE proofEmbedU #-}

proofEmbedS :: Elem e ts -> e -> SList ts
proofEmbedS EZ    x = SHead x
proofEmbedS (ES n) x = STail $ proofEmbedS n x
{-# INLINE proofEmbedS #-}

proofEmbedE :: Elem e ts -> e -> EList ts
proofEmbedE EZ    x = EHead x
proofEmbedE (ES n) x = ETail $ proofEmbedE n x
{-# INLINE proofEmbedE #-}

-- | Sum of two type-level lists.
hAdd :: HList as -> HList bs -> HList (as ++ bs)
hAdd Nil bs = bs
hAdd (x :* xs) bs = x :* hAdd xs bs

data ProofSubList (ys :: [Type]) (xs :: [Type]) where
  PSubNil :: ProofSubList '[] xs
  PSubAdd :: Elem y xs -> ProofSubList ys xs -> ProofSubList (y : ys) xs

proofGetSubList :: ProofSubList ys xs -> HList xs -> HList ys
proofGetSubList PSubNil _ = Nil
proofGetSubList (PSubAdd e t) xs = getE e xs :* proofGetSubList t xs
{-# INLINE proofGetSubList #-}

proofGetSubListF :: ProofSubList ys xs -> FList f xs -> FList f ys
proofGetSubListF PSubNil _ = FNil
proofGetSubListF (PSubAdd e t) xs = getEF e xs :** proofGetSubListF t xs
{-# INLINE proofGetSubListF #-}

proofSubListModify :: ProofSubList ys xs -> (HList ys -> HList ys) -> HList xs -> HList xs
proofSubListModify PSubNil _ xs = xs
proofSubListModify p@(PSubAdd e t) f xs =
    let hy :* hys = f (proofGetSubList p xs)
    in proofModifyH e (const hy) $ proofSubListUpdate t xs hys
{-# INLINE proofSubListModify #-}

proofSubListModifyF :: ProofSubList ys xs -> (FList f ys -> FList f ys) -> FList f xs -> FList f xs
proofSubListModifyF PSubNil _ xs = xs
proofSubListModifyF p@(PSubAdd e t) f xs =
    let hy :** hys = f (proofGetSubListF p xs)
    in proofModifyF e (const hy) $ proofSubListUpdateF t xs hys
{-# INLINE proofSubListModifyF #-}

proofSubListUpdate :: ProofSubList ys xs -> HList xs -> HList ys -> HList xs
proofSubListUpdate PSubNil xs _ = xs
proofSubListUpdate (PSubAdd e t) xs ys =
    let hy :* hys = ys
    in proofModifyH e (const hy) $ proofSubListUpdate t xs hys
{-# INLINE proofSubListUpdate #-}

proofSubListUpdateF :: ProofSubList ys xs -> FList f xs -> FList f ys -> FList f xs
proofSubListUpdateF PSubNil xs _ = xs
proofSubListUpdateF (PSubAdd e t) xs ys =
    let hy :** hys = ys
    in proofModifyF e (const hy) $ proofSubListUpdateF t xs hys
{-# INLINE proofSubListUpdateF #-}

proofSubListEmbed :: ProofSubList ys xs -> SList ys -> SList xs
proofSubListEmbed PSubNil _ = SEmpty
proofSubListEmbed (PSubAdd _ _) SEmpty = SEmpty
proofSubListEmbed (PSubAdd e _) (SHead y) = proofEmbedS e y
proofSubListEmbed (PSubAdd _ t) (STail ys) = proofSubListEmbed t ys
{-# INLINE proofSubListEmbed #-}

proofSubListErrorEmbed :: ProofSubList ys xs -> Result ys a -> Result xs a
proofSubListErrorEmbed PSubNil (RSuccess a) = RSuccess a
proofSubListErrorEmbed (PSubAdd _ _) (RSuccess a) = RSuccess a
proofSubListErrorEmbed (PSubAdd e _) (RFailure (EHead y)) = RFailure (proofEmbedE e y)
proofSubListErrorEmbed (PSubAdd _ t) (RFailure (ETail ys)) = proofSubListErrorEmbed t (RFailure ys)
{-# INLINE proofSubListErrorEmbed #-}

-- | A class for working with sublists.
class SubList (ys :: [Type]) (xs :: [Type]) where
  getSubList     :: HList xs   -> HList ys                                   -- ^ Get the sublist from the HList.
  getSubListF    :: FList f xs -> FList f ys                               -- ^ Get the sublist from the FList.
  subListModify  :: (HList ys  -> HList ys) -> HList xs -> HList xs         -- ^ Modify the sublist in the HList.
  subListModifyF :: (FList f ys -> FList f ys) -> FList f xs -> FList f xs -- ^ Modify the sublist in the FList.
  subListUpdate  :: HList xs   -> HList ys -> HList xs                       -- ^ Update the sublist in the HList.
  subListUpdate xs ys = subListModify (const ys) xs
  {-# INLINE subListUpdate #-}

  subListEmbed   :: SList ys -> SList xs

  subListErrorEmbed :: Result ys a -> Result xs a

  subListUpdateF :: FList f xs -> FList f ys -> FList f xs -- Update the sublist in the FList.
  subListUpdateF xs ys = subListModifyF (const ys) xs
  {-# INLINE subListUpdateF #-}

-- | A class for embedding sublists.
class EmbedSubList (ys :: [Type]) (xs :: [Type]) where
  embedSubList :: UList f ys -> UList f xs -- Embed a UList of ys into a UList of xs.

instance EmbedSubList '[] '[] where
  embedSubList = id
  {-# INLINE embedSubList #-}

instance EmbedSubList '[] xs => EmbedSubList '[] (x : xs) where
  embedSubList _ = UTail $ embedSubList @'[] @xs UNil
  {-# INLINE embedSubList #-}

instance (EmbedSubList ys xs, In y xs) => EmbedSubList (y : ys) xs where
  embedSubList (UHead y)  = embedU y
  embedSubList (UTail ys) = embedSubList ys
  {-# INLINE embedSubList #-}

instance {-# OVERLAPPING #-} EmbedSubList ys (y : ys) where
  {-# INLINE embedSubList #-}
  embedSubList = UTail

instance SubList '[] xs where
  getSubList _ = Nil
  {-# INLINE getSubList #-}
  getSubListF _ = FNil
  {-# INLINE getSubListF #-}
  subListModify _ xs = xs
  {-# INLINE subListModify #-}
  subListModifyF _ xs = xs
  {-# INLINE subListModifyF #-}
  subListEmbed _ = SEmpty
  {-# INLINE subListEmbed #-}
  subListErrorEmbed (RSuccess a) = RSuccess a
  {-# INLINE subListErrorEmbed #-}

-- | Induction case for the SubList class.
instance (In y xs, SubList ys xs) => SubList (y : ys) xs where
  getSubList xs  = getH xs :* getSubList xs
  {-# INLINE getSubList #-}
  getSubListF xs = getF xs :** getSubListF xs
  {-# INLINE getSubListF #-}
  subListModify f xs =
    let hy :* hys = f (getSubList xs)
    in modifyH (const hy) $ subListUpdate xs hys
  {-# INLINE subListModify #-}
  subListModifyF f xs =
    let hy :** hys = f (getSubListF xs)
    in modifyF (const hy) $ subListUpdateF xs hys
  {-# INLINE subListModifyF #-}
  subListEmbed SEmpty     = SEmpty
  subListEmbed (SHead y)  = embedS y
  subListEmbed (STail ys) = subListEmbed ys
  {-# INLINE subListEmbed #-}
  subListErrorEmbed (RSuccess a)          = RSuccess a
  subListErrorEmbed (RFailure (EHead y))  = RFailure (embedE y)
  subListErrorEmbed (RFailure (ETail ys)) = subListErrorEmbed (RFailure ys)
  {-# INLINE subListErrorEmbed #-}

instance {-# INCOHERENT #-} SubList xs xs where
  getSubList = id
  {-# INLINE getSubList #-}
  getSubListF = id
  {-# INLINE getSubListF #-}
  subListModify = id
  {-# INLINE subListModify #-}
  subListModifyF = id
  {-# INLINE subListModifyF #-}
  subListEmbed = id
  {-# INLINE subListEmbed #-}
  subListErrorEmbed = id
  {-# INLINE subListErrorEmbed #-}
