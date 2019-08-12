{-# LANGUAGE ExistentialQuantification #-}
module Callable where

import {-# SOURCE #-} Value
import {-# SOURCE #-} Interpreter
import {-# SOURCE #-} Class

class Callable c where
  arity :: c -> Int
  call :: c -> Interpreter -> [Value] -> IO Value
  toString :: c -> String
  isClass :: c -> Maybe LoxClass
  isClass = const Nothing

data MkCallable = forall c. Callable c => MkCallable c

instance Callable MkCallable where
  arity (MkCallable c) = arity c
  call (MkCallable c) = call c
  toString (MkCallable c) = toString c
  isClass (MkCallable c) = isClass c
