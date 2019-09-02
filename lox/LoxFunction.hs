{-# LANGUAGE OverloadedStrings #-}
module LoxFunction (LoxFunction,newFunction,bind) where

import Control.Monad
import Data.Unique
import Data.Dynamic
import qualified Data.Text as T
import Control.Monad.Except

import Stmt
import Token (tokenLexeme)
import LoxCallable (LoxCallable,arity,call,callableId,toString)
import Environment (Environment,childEnvironment,define,getAt)
import Return (Return(Return))
import {-# SOURCE #-} Interpreter (executeBlock)
import {-# SOURCE #-} LoxInstance (LoxInstance)

data LoxFunction = LoxFunction { lfunDeclaration :: FunDecl, lfunClosure :: Environment, lfunIsInitializer :: Bool, lfunId :: Unique }
instance LoxCallable LoxFunction where
  arity (LoxFunction (FunDecl _ params _) _ _ _) = length params
  call (LoxFunction (FunDecl _ params body) closure isInitializer _) arguments = do
    environment <- liftIO $ childEnvironment closure
    liftIO $ forM_ (zip params arguments) $ \(p,a) ->
      define (tokenLexeme p) a environment
    flip catchError (\(Return.Return v) -> liftIO $ if isInitializer
                                                    then getAt 0 "this" closure
                                                    else return v) $ do
      executeBlock body environment
      liftIO $ if isInitializer then getAt 0 "this" closure else return (toDyn ())
  callableId = lfunId
  toString (LoxFunction (FunDecl name _ _) _ _ _) = T.concat ["<fn ",tokenLexeme name,">"]

newFunction :: FunDecl -> Environment -> Bool -> IO LoxFunction
newFunction decl closure isInit = LoxFunction decl closure isInit <$> newUnique

bind :: LoxFunction -> LoxInstance -> IO LoxFunction
bind f inst = do
  e <- childEnvironment (lfunClosure f)
  define "this" (toDyn inst) e
  newFunction (lfunDeclaration f) e (lfunIsInitializer f)
