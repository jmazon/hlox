module Function where

import Control.Exception
import Control.Monad

import Stmt
import Token
import Callable
import Environment
import ReturnException
import {-# SOURCE #-} Interpreter
import {-# SOURCE #-} Value
import {-# SOURCE #-} Instance

data LoxFunction = LoxFunction { lfunDeclaration :: Stmt, lfunClosure :: Environment, lfunIsInitializer :: Bool }
instance Callable LoxFunction where
  arity (LoxFunction (Function _ params _) _ _) = length params
  call (LoxFunction (Function name params body) closure isInitializer) i arguments = do
    environment <- childEnvironment closure
    forM_ (zip params arguments) $ \(p,a) ->
      define environment (tokenLexeme p) a
    handle (\(ReturnException v) -> if isInitializer
                                    then getAt closure 0 "this"
                                    else return v) $ do
      executeBlock i body environment
      if isInitializer then getAt closure 0 "this" else return VNull
  toString (LoxFunction (Function name _ _) _ _) = "<fn " ++ tokenLexeme name ++ ">"

newFunction :: Stmt -> Environment -> Bool -> LoxFunction
newFunction = LoxFunction

bind :: LoxFunction -> LoxInstance -> IO LoxFunction
bind f inst = do
  e <- childEnvironment (lfunClosure f)
  define e "this" (VInstance inst)
  return $ newFunction (lfunDeclaration f) e (lfunIsInitializer f)
