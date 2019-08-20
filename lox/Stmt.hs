{-# LANGUAGE DeriveGeneric #-}
module Stmt where

import GHC.Generics (Generic)
import Token (Token)
import Expr

data Stmt =
    Block { blockStatements :: [Stmt] }
  | Class { className :: Token, classSuperclass :: Maybe Expr, classMethods :: [FunDecl] }
  | Expression { expressionExpression :: Expr }
  | Function { functionDecl :: FunDecl }
  | If { ifCondition :: Expr, ifThenbranch :: Stmt, ifElsebranch :: Maybe Stmt }
  | Print { printExpression :: Expr }
  | Return { returnKeyword :: Token, returnValue :: Maybe Expr }
  | Var { varName :: Token, varInitializer :: Maybe Expr }
  | While { whileCondition :: Expr, whileBody :: Stmt }
  deriving (Show,Eq,Generic)

data FunDecl = FunDecl { functionName :: Token, functionParams :: [Token], functionBody :: [Stmt] }
               deriving (Show,Eq,Generic)
