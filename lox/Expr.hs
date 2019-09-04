{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Expr where

import Data.Hashable
import Data.Dynamic

import Token (Token)

data Expr =
    Assign { assignName :: Token, assignId :: !ExprKey, assignValue :: Expr }
  | Binary { binaryLeft :: Expr, binaryToken :: !Token, binaryOperator :: !BinaryOp, binaryRight :: Expr }
  | Call { callCallee :: Expr, callParen :: Token, callArguments :: [Expr] }
  | Get { getObject :: Expr, getName :: Token }
  | Grouping { groupingExpression :: Expr }
  | Literal { literalValue :: Dynamic }
  | Logical { logicalLeft :: Expr, logicalOperator :: !LogicalOp, logicalRight :: Expr }
  | Set { setObject :: Expr, setName :: Token, setValue :: Expr }
  | Super { superKeyword :: Token, superId :: !ExprKey, superMethod :: Token }
  | This { thisKeyword :: Token, thisId :: !ExprKey }
  | Unary { unaryOperator :: !UnaryOp, unaryToken :: !Token, unaryRight :: Expr }
  | Variable { variableName :: Token, variableId :: !ExprKey }

newtype ExprKey = ExprKey Int deriving (Eq,Hashable)

data UnaryOp = UnaryBang | UnaryMinus
data BinaryOp = BinGreater | BinGreaterEqual
              | BinLess | BinLessEqual
              | BinBangEqual | BinEqualEqual
              | BinPlus | BinMinus
              | BinSlash | BinStar
data LogicalOp = LogOr | LogAnd
