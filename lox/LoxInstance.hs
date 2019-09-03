{-# LANGUAGE OverloadedStrings #-}
module LoxInstance (LoxInstance(LoxInstance),instanceClass,newInstance,getP,setP) where

import Data.IORef
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Control.Exception
import Data.Dynamic
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Trans

import Token (Token,tokenLexeme)
import LoxClass (LoxClass,findMethod)
import RuntimeError (RuntimeError(RuntimeError))
import LoxFunction (bind)

data LoxInstance = LoxInstance { instanceClass :: LoxClass
                               , instanceFields :: IORef (HashMap Text Dynamic)}

newInstance :: MonadIO m => LoxClass -> m LoxInstance
newInstance c = LoxInstance c <$> liftIO (newIORef H.empty)

getP :: MonadIO m => LoxInstance -> Token -> m Dynamic
getP i name = do
  r <- H.lookup (tokenLexeme name) <$> liftIO (readIORef (instanceFields i))
  case r of Just v -> return v
            Nothing -> case findMethod (instanceClass i) (tokenLexeme name) of
              Just m -> toDyn <$> bind m i
              Nothing -> liftIO (throwIO (RuntimeError name (T.concat ["Undefined property '",tokenLexeme name,"'."])))

setP :: MonadIO m => LoxInstance -> Token -> Dynamic -> m ()
setP i name value =
  liftIO $ modifyIORef (instanceFields i) (H.insert (tokenLexeme name) value)
