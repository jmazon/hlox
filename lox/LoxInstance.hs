{-# LANGUAGE OverloadedStrings #-}
module LoxInstance (LoxInstance(LoxInstance),instanceClass,newInstance,getP,setP) where

import Data.IORef
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Control.Exception
import Data.Dynamic
import Data.Text (Text)
import qualified Data.Text as T

import Token (Token,tokenLexeme)
import LoxClass (LoxClass,findMethod)
import RuntimeError (RuntimeError(RuntimeError))
import LoxFunction (bind)

data LoxInstance = LoxInstance { instanceClass :: LoxClass
                               , instanceFields :: IORef (HashMap Text Dynamic)}

newInstance :: LoxClass -> IO LoxInstance
newInstance c = LoxInstance c <$> newIORef H.empty

getP :: LoxInstance -> Token -> IO Dynamic
getP i name = do
  r <- H.lookup (tokenLexeme name) <$> readIORef (instanceFields i)
  case r of Just v -> return v
            Nothing -> case findMethod (instanceClass i) (tokenLexeme name) of
              Just m -> toDyn <$> bind m i
              Nothing -> throwIO (RuntimeError name (T.concat ["Undefined property '",tokenLexeme name,"'."]))

setP :: LoxInstance -> Token -> Dynamic -> IO ()
setP i name value =
  modifyIORef (instanceFields i) (H.insert (tokenLexeme name) value)
