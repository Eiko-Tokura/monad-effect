{-# LANGUAGE DeriveAnyClass #-}
module TH where

import Module.RS.QQ

[makeRModule|
MyModule
  field1 :: !Int
  Lens field2 :: Bool
|]

[makeRSModule|
MyRSModule
  Lens readField :: !Int
  State stateField :: !Int
|]
