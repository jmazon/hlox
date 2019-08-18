module LoxFunction where

import Control.Exception
import Control.Monad
import Data.Unique
import Data.Dynamic

import Stmt
import Token
import LoxCallable
import Environment
import Return
import {-# SOURCE #-} Interpreter
import {-# SOURCE #-} LoxInstance

data LoxFunction = LoxFunction { lfunDeclaration :: Stmt, lfunClosure :: Environment, lfunIsInitializer :: Bool, lfunId :: Unique }
instance LoxCallable LoxFunction where
  arity (LoxFunction (Function _ params _) _ _ _) = length params
  call (LoxFunction (Function name params body) closure isInitializer _) i arguments = do
    environment <- childEnvironment closure
    forM_ (zip params arguments) $ \(p,a) ->
      define environment (tokenLexeme p) a
    handle (\(Return.Return v) -> if isInitializer
                                    then getAt closure 0 "this"
                                    else return v) $ do
      executeBlock i body environment
      if isInitializer then getAt closure 0 "this" else return (toDyn ())
  callableId = lfunId
  toString (LoxFunction (Function name _ _) _ _ _) = "<fn " ++ tokenLexeme name ++ ">"

newFunction :: Stmt -> Environment -> Bool -> IO LoxFunction
newFunction decl closure isInit = LoxFunction decl closure isInit <$> newUnique

bind :: LoxFunction -> LoxInstance -> IO LoxFunction
bind f inst = do
  e <- childEnvironment (lfunClosure f)
  define e "this" (toDyn inst)
  newFunction (lfunDeclaration f) e (lfunIsInitializer f)
