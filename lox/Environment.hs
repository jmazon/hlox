module Environment where

import Data.IORef
import qualified Data.HashMap.Strict as H
import Control.Exception

import Token
import Value
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
