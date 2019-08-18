{-# LANGUAGE ExistentialQuantification #-}
module LoxCallable where

import Data.Unique
import Data.Dynamic

import {-# SOURCE #-} Interpreter

class LoxCallable c where
  arity :: c -> Int
  call :: c -> Interpreter -> [Dynamic] -> IO Dynamic
  toString :: c -> String
  callableId :: c -> Unique
