module Environment where

import Data.Maybe
import Data.IORef
import qualified Data.HashMap.Strict as H
import Control.Exception

import Token
import {-# SOURCE #-} Value
import RuntimeError

data Environment = Environment {
    environmentEnclosing :: Maybe Environment
  , environmentValues :: IORef (H.HashMap String Value)
  }

newEnvironment :: IO Environment
newEnvironment = Environment Nothing <$> newIORef H.empty

childEnvironment :: Environment -> IO Environment
childEnvironment e = Environment (Just e) <$> newIORef H.empty

get :: Environment -> Token -> IO Value
get e name = do
  r <- H.lookup (tokenLexeme name) <$> readIORef (environmentValues e)
  case (r,environmentEnclosing e) of
    (Just v,_) -> return v
    (Nothing,Just e') -> get e' name
    (Nothing,Nothing) -> throwIO (RuntimeError name ("Undefined variable '" ++ tokenLexeme name ++ "'."))

assign :: Environment -> Token -> Value -> IO ()
assign e name value = do
  c <- H.member (tokenLexeme name) <$> readIORef (environmentValues e)
  case (c,environmentEnclosing e) of
    (True,_) -> modifyIORef (environmentValues e)
                            (H.insert (tokenLexeme name) value)
    (False,Just e') -> assign e' name value
    (False,Nothing) -> throwIO (RuntimeError name ("Undefined variable '" ++ tokenLexeme name ++ "'."))

define :: Environment -> String -> Value -> IO ()
define e name value = modifyIORef (environmentValues e) (H.insert name value)

ancestor :: Environment -> Int -> Environment
ancestor e 0 = e
ancestor e i = ancestor (fromJust $ environmentEnclosing e) (i-1)

getAt :: Environment -> Int -> String -> IO Value
getAt e d name = fmap (H.! name) $ readIORef $ environmentValues $ ancestor e d

assignAt :: Environment -> Int -> Token -> Value -> IO ()
assignAt e d name value =
  modifyIORef (environmentValues (ancestor e d)) (H.insert (tokenLexeme name) value)
