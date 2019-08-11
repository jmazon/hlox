{-# LANGUAGE ExistentialQuantification #-}
module Callable where

import {-# SOURCE #-} Value
import {-# SOURCE #-} Interpreter

class Callable c where
  arity :: c -> Int
  call :: c -> Interpreter -> [Value] -> IO Value
  toString :: c -> String

data MkCallable = forall c. Callable c => MkCallable c

instance Callable MkCallable where
  arity (MkCallable c) = arity c
  call (MkCallable c) = call c
  toString (MkCallable c) = toString c
