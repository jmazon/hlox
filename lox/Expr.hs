module Expr where

import Token (Token,Literal)
import Data.Unique

data Expr =
    Assign { assignName :: Token, assignId :: Unique, assignValue :: Expr }
  | Binary { binaryLeft :: Expr, binaryOperator :: Token, binaryRight :: Expr }
  | Call { callCallee :: Expr, callParen :: Token, callArguments :: [Expr] }
  | Get { getObject :: Expr, getName :: Token }
  | Grouping { groupingExpression :: Expr }
  | Literal { literalValue :: Literal }
  | Logical { logicalLeft :: Expr, logicalOperator :: Token, logicalRight :: Expr }
  | Set { setObject :: Expr, setName :: Token, setValue :: Expr }
  | Super { superKeyword :: Token, superId :: Unique, superMethod :: Token }
  | This { thisKeyword :: Token, thisId :: Unique }
  | Unary { unaryOperator :: Token, unaryRight :: Expr }
  | Variable { variableName :: Token, variableId :: Unique }

