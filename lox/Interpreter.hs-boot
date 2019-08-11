module Interpreter where

import Expr
import Stmt
import Environment

data Interpreter

resolveI :: Interpreter -> Expr -> Int -> IO ()
executeBlock :: Interpreter -> [Stmt] -> Environment -> IO ()
