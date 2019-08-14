{-# LANGUAGE ExistentialQuantification #-}
module LoxCallable where

import Data.Unique

import {-# SOURCE #-} Value
import {-# SOURCE #-} Interpreter
import {-# SOURCE #-} LoxClass

class Callable c where
  arity :: c -> Int
  call :: c -> Interpreter -> [Value] -> IO Value
  toString :: c -> String
  callableId :: c -> Unique
  isClass :: c -> Maybe LoxClass
  isClass = const Nothing

data MkCallable = forall c. Callable c => MkCallable c

instance Callable MkCallable where
  arity (MkCallable c) = arity c
  call (MkCallable c) = call c
  toString (MkCallable c) = toString c
  callableId (MkCallable c) = callableId c
  isClass (MkCallable c) = isClass c
