{-# LANGUAGE ExistentialQuantification #-}
module LoxCallable (LoxCallable(..)) where

import Data.Unique
import Data.Dynamic
import Data.Text (Text)

import {-# SOURCE #-} Interpreter (Interpreter)

class LoxCallable c where
  arity :: c -> Int
  call :: c -> Interpreter -> [Dynamic] -> IO Dynamic
  toString :: c -> Text
  callableId :: c -> Unique
