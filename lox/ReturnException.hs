module ReturnException where

import Control.Exception

import {-# SOURCE #-} Value

data ReturnException = ReturnException Value
instance Exception ReturnException
instance Show ReturnException where
  show = error "Undefined Show instance for ReturnException"
