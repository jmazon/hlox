{-# LANGUAGE ExistentialQuantification #-}
module Value where

import {-# SOURCE #-} Interpreter

data Value = VNull | VNumber Double | VBool Bool | VString String
           | VCallable MkCallable
instance Eq Value where
  VNull == VNull = True
  VNumber a == VNumber b = a == b
  VBool a == VBool b = a == b
  VString a == VString b = a == b
  _ == _ = False

isCallable (VCallable _) = True
isCallable _ = False

class Callable c where
  arity :: c -> Int
  call :: c -> Interpreter -> [Value] -> IO Value
  toString :: c -> String

data MkCallable = forall c. Callable c => MkCallable c

instance Callable MkCallable where
  arity (MkCallable c) = arity c
  call (MkCallable c) = call c
  toString (MkCallable c) = toString c
