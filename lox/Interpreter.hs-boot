module Interpreter (Interpreter,executeBlock) where

import Stmt
import Environment (Environment)

data Interpreter

executeBlock :: Interpreter -> [Stmt] -> Environment -> IO ()
