{-# LANGUAGE OverloadedStrings #-}
module Environment (Environment,newEnvironment,childEnvironment,define,getAt,assignAt,assign,get) where

import Data.Maybe
import Data.IORef
import qualified Data.HashMap.Strict as H
import Control.Exception
import Data.Dynamic
import Data.Text (Text)
import qualified Data.Text as T

import Token (Token,tokenLexeme)
import RuntimeError (RuntimeError(RuntimeError))

data Environment = Environment {
    environmentEnclosing :: Maybe Environment
  , environmentValues :: IORef (H.HashMap Text Dynamic)
  }

newEnvironment :: IO Environment
newEnvironment = Environment Nothing <$> newIORef H.empty

childEnvironment :: Environment -> IO Environment
childEnvironment e = Environment (Just e) <$> newIORef H.empty

get :: Token -> Environment -> IO Dynamic
get name e = do
  r <- H.lookup (tokenLexeme name) <$> readIORef (environmentValues e)
  case (r,environmentEnclosing e) of
    (Just v,_) -> return v
    (Nothing,Just e') -> get name e'
    (Nothing,Nothing) -> throwIO (RuntimeError name (T.concat ["Undefined variable '",tokenLexeme name,"'."]))

assign :: Token -> Dynamic -> Environment -> IO ()
assign name value e = do
  c <- H.member (tokenLexeme name) <$> readIORef (environmentValues e)
  case (c,environmentEnclosing e) of
    (True,_) -> modifyIORef (environmentValues e)
                            (H.insert (tokenLexeme name) value)
    (False,Just e') -> assign name value e'
    (False,Nothing) -> throwIO (RuntimeError name (T.concat ["Undefined variable '",tokenLexeme name,"'."]))

define :: Text -> Dynamic -> Environment -> IO ()
define name value e = modifyIORef (environmentValues e) (H.insert name value)

ancestor :: Environment -> Int -> Environment
ancestor e 0 = e
ancestor e i = ancestor (fromJust $ environmentEnclosing e) (i-1)

getAt :: Int -> Text -> Environment -> IO Dynamic
getAt d name e = fmap (H.! name) $ readIORef $ environmentValues $ ancestor e d

assignAt :: Int -> Token -> Dynamic -> Environment -> IO ()
assignAt d name value e =
  modifyIORef (environmentValues (ancestor e d)) (H.insert (tokenLexeme name) value)
