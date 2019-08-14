{-# LANGUAGE ExistentialQuantification #-}
module Value where

import {-# SOURCE #-} LoxInstance
import {-# SOURCE #-} LoxCallable

data Value = VNull | VNumber Double | VBool Bool | VString String
           | VCallable MkCallable | VInstance LoxInstance
instance Eq Value where
  VNull == VNull = True
  VNumber a == VNumber b = a == b
  VBool a == VBool b = a == b
  VString a == VString b = a == b
  VCallable a == VCallable b = callableId a == callableId b
  _ == _ = False

isCallable (VCallable _) = True
isCallable _ = False
