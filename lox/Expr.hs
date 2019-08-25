{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Expr where

import Data.Hashable

import Token (Token,Literal)

data Expr =
    Assign { assignName :: Token, assignId :: !ExprKey, assignValue :: Expr }
  | Binary { binaryLeft :: Expr, binaryOperator :: Token, binaryRight :: Expr }
  | Call { callCallee :: Expr, callParen :: Token, callArguments :: [Expr] }
  | Get { getObject :: Expr, getName :: Token }
  | Grouping { groupingExpression :: Expr }
  | Literal { literalValue :: Literal }
  | Logical { logicalLeft :: Expr, logicalOperator :: Token, logicalRight :: Expr }
  | Set { setObject :: Expr, setName :: Token, setValue :: Expr }
  | Super { superKeyword :: Token, superId :: !ExprKey, superMethod :: Token }
  | This { thisKeyword :: Token, thisId :: !ExprKey }
  | Unary { unaryOperator :: Token, unaryRight :: Expr }
  | Variable { variableName :: Token, variableId :: !ExprKey }

newtype ExprKey = ExprKey Int deriving (Eq,Hashable)
