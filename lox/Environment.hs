{-# LANGUAGE OverloadedStrings #-}
module Environment (Environment,newEnvironment,childEnvironment,define,getAt,assignAt,assign,get) where

import Data.Maybe
import Data.IORef
import qualified Data.HashMap.Strict as H
import Control.Exception
import Data.Dynamic
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Trans

import Token (Token,tokenLexeme)
import RuntimeError (RuntimeError(RuntimeError))

data Environment = Environment {
    environmentEnclosing :: Maybe Environment
  , environmentValues :: IORef (H.HashMap Text Dynamic)
  }

newEnvironment :: IO Environment
newEnvironment = Environment Nothing <$> newIORef H.empty

childEnvironment :: MonadIO m => Environment -> m Environment
childEnvironment e = Environment (Just e) <$> liftIO (newIORef H.empty)

get :: MonadIO m => Token -> Environment -> m Dynamic
get name e = do
  r <- H.lookup (tokenLexeme name) <$> liftIO (readIORef (environmentValues e))
  case (r,environmentEnclosing e) of
    (Just v,_) -> return v
    (Nothing,Just e') -> get name e'
    (Nothing,Nothing) -> liftIO (throwIO (RuntimeError name (T.concat ["Undefined variable '",tokenLexeme name,"'."])))

assign :: MonadIO m => Token -> Dynamic -> Environment -> m ()
assign name value e = do
  c <- H.member (tokenLexeme name) <$> liftIO (readIORef (environmentValues e))
  case (c,environmentEnclosing e) of
    (True,_) -> liftIO $ modifyIORef (environmentValues e)
                                     (H.insert (tokenLexeme name) value)
    (False,Just e') -> assign name value e'
    (False,Nothing) -> liftIO (throwIO (RuntimeError name (T.concat ["Undefined variable '",tokenLexeme name,"'."])))

define :: MonadIO m => Text -> Dynamic -> Environment -> m ()
define name value e = liftIO $ modifyIORef (environmentValues e) (H.insert name value)

ancestor :: Environment -> Int -> Environment
ancestor e 0 = e
ancestor e i = ancestor (fromJust $ environmentEnclosing e) (i-1)

getAt :: MonadIO m => Int -> Text -> Environment -> m Dynamic
getAt d name e = fmap (H.! name) $ liftIO . readIORef $ environmentValues $ ancestor e d

assignAt :: MonadIO m => Int -> Token -> Dynamic -> Environment -> m ()
assignAt d name value e =
  liftIO $ modifyIORef (environmentValues (ancestor e d)) (H.insert (tokenLexeme name) value)
