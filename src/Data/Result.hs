-- | Module      : Data.Result
-- Description : A module that provides a Result type for handling errors in a non-empty list of types.
module Data.Result where

import Data.Kind (Type)
import Data.TypeList.Families

-- | Sum of types, but non-empty by construction.
-- You cannot construct EList '[]
data EList (ts :: [Type]) where
  EHead :: !t -> EList (t : ts)
  ETail :: !(EList ts) -> EList (t : ts)

embedES :: SFirstIndex e ts -> e -> EList ts
embedES SFirstIndexZero e = EHead e
embedES (SFirstIndexSucc _ n) e = ETail (embedES n e)
{-# INLINE embedES #-}

-- | Error type that adapt to the type-level list of errors.
data Result (es :: [Type]) (a :: Type) where
  RSuccess :: a -> Result es a
  RFailure :: !(EList es) -> Result es a

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
  return = pure
  {-# INLINE return #-}

getEMaybeS :: SFirstIndex e es -> EList es -> Maybe e
getEMaybeS SFirstIndexZero = \case
  EHead x  -> Just x
  ETail _  -> Nothing
getEMaybeS (SFirstIndexSucc _ n) = \case
  EHead _  -> Nothing
  ETail es -> getEMaybeS n es
{-# INLINE getEMaybeS #-}

resultNoError :: Result '[] a -> a
resultNoError (RSuccess a) = a
{-# INLINE resultNoError #-}

resultToEither :: Result (e ': es) a -> Either (EList (e ': es)) a
resultToEither (RSuccess a)  = Right a
resultToEither (RFailure es) = Left es
{-# INLINE resultToEither #-}

resultMapErrors :: (EList es -> EList es') -> Result es a -> Result es' a
resultMapErrors _ (RSuccess a)  = RSuccess a
resultMapErrors f (RFailure es) = RFailure (f es)
{-# INLINE resultMapErrors #-}

fromElistSingleton :: EList '[s] -> s
fromElistSingleton (EHead x) = x
{-# INLINE fromElistSingleton #-}

getElemRemoveResult :: SNat n -> Result es a -> Either (AtIndex es n) (Result (Remove n es) a)
getElemRemoveResult _         (RSuccess a)          = Right (RSuccess a)
getElemRemoveResult SZero     (RFailure (EHead e))  = Left e
getElemRemoveResult SZero     (RFailure (ETail es)) = Right (RFailure es)
getElemRemoveResult (SSucc _) (RFailure (EHead e))  = Right $ RFailure $ EHead e
getElemRemoveResult (SSucc n) (RFailure (ETail es)) =
  case getElemRemoveResult n (RFailure es) of
    Left  e              -> Left e
    Right (RFailure es') -> Right (RFailure $ ETail es')
    Right (RSuccess a)   -> Right (RSuccess a)
{-# INLINE getElemRemoveResult #-}
