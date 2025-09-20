{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
module Data.TypeList.ConsFData.Pattern where

import Data.TypeList.ConsFData
import Data.TypeList.Families

pattern (:**) :: ConsFData flist => f t -> flist f ts -> flist f (t : ts)
pattern (:**) x xs <- (unConsF -> (x, xs)) where
  y :** ys = consF y ys
{-# COMPLETE (:**) #-}
infixr 1 :**

pattern (:***) :: (NonEmpty ts ~ True, ConsFDataList flist ts) => f (Head ts) -> flist f (Tail ts) -> flist f ts
pattern (:***) x xs <- (unConsFData -> (x, xs)) where
  y :*** ys = consF0 y ys
{-# COMPLETE (:***) #-}
infixr 1 :***
