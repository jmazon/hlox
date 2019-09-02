{-# LANGUAGE ExistentialQuantification #-}
module LoxCallable (LoxCallable(..)) where

import Data.Unique
import Data.Dynamic
import Data.Text (Text)

import {-# SOURCE #-} Interpreter (MI)

class LoxCallable c where
  arity :: c -> Int
  call :: c -> [Dynamic] -> MI Dynamic
  toString :: c -> Text
  callableId :: c -> Unique
