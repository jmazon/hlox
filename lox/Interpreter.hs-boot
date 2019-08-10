module Interpreter where

import Expr

data Interpreter

resolveI :: Interpreter -> Expr -> Int -> IO ()