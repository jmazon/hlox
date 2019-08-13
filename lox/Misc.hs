module Misc where

import Control.Exception
import System.IO

import Globals
import qualified TokenType as TT
import Token
import RuntimeError

loxError :: Int -> String -> IO ()
loxError line message = do
  report line "" message

report :: Int -> String -> String -> IO ()
report line location message = do
  hPutStrLn stderr $ "[line " ++ show line ++ "] Error" ++ location
                     ++ ": " ++ message
  writeGlobal hadError True

data ParseError = ParseError deriving Show
instance Exception ParseError

tokenError :: Token -> String -> IO ParseError
tokenError t m = do
  if tokenType t == TT.Eof
    then report (tokenLine t) " at end" m
    else report (tokenLine t) (" at '" ++ tokenLexeme t ++ "'") m
  return ParseError

runtimeError :: RuntimeError -> IO ()
runtimeError (RuntimeError token message) = do
  hPutStrLn stderr $ message ++ "\n[line " ++ show (tokenLine token) ++ "]"
  writeGlobal hadRuntimeError True
