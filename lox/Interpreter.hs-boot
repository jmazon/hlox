module Interpreter (Interpreter,executeBlock,resolveI) where

import Expr
import Stmt
import Environment (Environment)

data Interpreter

resolveI :: Interpreter -> Expr -> Int -> IO ()
executeBlock :: Interpreter -> [Stmt] -> Environment -> IO ()
