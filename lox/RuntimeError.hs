module RuntimeError (RuntimeError(RuntimeError)) where

import Control.Exception
import Data.Text (Text)

import Token (Token)

data RuntimeError = RuntimeError Token Text deriving Show
instance Exception RuntimeError
