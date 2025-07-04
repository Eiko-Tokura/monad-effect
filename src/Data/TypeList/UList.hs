module Data.TypeList.UList where

import Data.Kind (Type)

-- | A type-level list applied to a type-level function, representing a sum.
data UList (f :: Type -> Type) (ts :: [Type]) where
  UNil  :: UList f '[]
  UHead :: f t -> UList f (t : ts)
  UTail :: UList f ts -> UList f (t : ts)
