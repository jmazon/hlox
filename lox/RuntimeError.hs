module RuntimeError (RuntimeError(RuntimeError)) where

import Control.Exception

import Token (Token)

data RuntimeError = RuntimeError Token String deriving Show
instance Exception RuntimeError
