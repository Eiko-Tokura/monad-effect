{-# LANGUAGE DeriveAnyClass #-}
module TH where

import Module.RS.QQ

[makeRModule|
MyModule
  field1 :: !Int
  field2 :: Bool
|]

[makeRSModule|
MyRSModule
  readField :: !Int
  State stateField :: !Int
|]
