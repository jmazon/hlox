module LoxInstance where

import Data.IORef
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Control.Exception
import Data.Dynamic

import Token
import LoxClass
import RuntimeError
import LoxCallable
import LoxFunction

data LoxInstance = LoxInstance { instanceClass :: LoxClass
                               , instanceFields :: IORef (HashMap String Dynamic)}

newInstance :: LoxClass -> IO LoxInstance
newInstance c = LoxInstance c <$> newIORef H.empty

getP :: LoxInstance -> Token -> IO Dynamic
getP i name = do
  r <- H.lookup (tokenLexeme name) <$> readIORef (instanceFields i)
  case r of Just v -> return v
            Nothing -> do
              case findMethod (instanceClass i) (tokenLexeme name) of
                Just m -> toDyn <$> bind m i
                Nothing -> throwIO (RuntimeError name ("Undefined property '" ++ tokenLexeme name ++ "'."))

setP :: LoxInstance -> Token -> Dynamic -> IO ()
setP i name value = do
  modifyIORef (instanceFields i) (H.insert (tokenLexeme name) value)
