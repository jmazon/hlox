module RuntimeError where

import Control.Exception

import Token

data RuntimeError = RuntimeError Token String deriving Show
instance Exception RuntimeError
