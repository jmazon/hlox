module Environment where

import Data.Maybe
import Data.IORef
import qualified Data.HashMap.Strict as H
import Control.Exception
import Data.Dynamic

import Token
import RuntimeError

data Environment = Environment {
    environmentEnclosing :: Maybe Environment
  , environmentValues :: IORef (H.HashMap String Dynamic)
  }

newEnvironment :: IO Environment
newEnvironment = Environment Nothing <$> newIORef H.empty

childEnvironment :: Environment -> IO Environment
childEnvironment e = Environment (Just e) <$> newIORef H.empty

get :: Environment -> Token -> IO Dynamic
get e name = do
  r <- H.lookup (tokenLexeme name) <$> readIORef (environmentValues e)
  case (r,environmentEnclosing e) of
    (Just v,_) -> return v
    (Nothing,Just e') -> get e' name
    (Nothing,Nothing) -> throwIO (RuntimeError name ("Undefined variable '" ++ tokenLexeme name ++ "'."))

assign :: Environment -> Token -> Dynamic -> IO ()
assign e name value = do
  c <- H.member (tokenLexeme name) <$> readIORef (environmentValues e)
  case (c,environmentEnclosing e) of
    (True,_) -> modifyIORef (environmentValues e)
                            (H.insert (tokenLexeme name) value)
    (False,Just e') -> assign e' name value
    (False,Nothing) -> throwIO (RuntimeError name ("Undefined variable '" ++ tokenLexeme name ++ "'."))

define :: Environment -> String -> Dynamic -> IO ()
define e name value = modifyIORef (environmentValues e) (H.insert name value)

ancestor :: Environment -> Int -> Environment
ancestor e 0 = e
ancestor e i = ancestor (fromJust $ environmentEnclosing e) (i-1)

getAt :: Environment -> Int -> String -> IO Dynamic
getAt e d name = fmap (H.! name) $ readIORef $ environmentValues $ ancestor e d

assignAt :: Environment -> Int -> Token -> Dynamic -> IO ()
assignAt e d name value =
  modifyIORef (environmentValues (ancestor e d)) (H.insert (tokenLexeme name) value)
