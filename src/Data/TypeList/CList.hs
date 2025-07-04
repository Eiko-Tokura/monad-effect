module Data.CList where

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

