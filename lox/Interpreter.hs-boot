module Interpreter (Interpreter,executeBlock,MI) where

import Control.Monad.Reader
import Control.Monad.Except

import Stmt
import Environment (Environment)
import Return

data Interpreter
type MI = ExceptT Return (ReaderT Interpreter IO)

executeBlock :: [Stmt] -> Environment -> MI ()
