module Return where

import Control.Exception

import {-# SOURCE #-} Value

data Return = Return Value
instance Exception Return
instance Show Return where
  show = error "Undefined Show instance for Return"
