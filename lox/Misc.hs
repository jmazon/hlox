module Misc where

import System.IO

import Globals
import Token
import RuntimeError

loxError :: Int -> String -> IO ()
loxError line message = do
  report line "" message

report :: Int -> String -> String -> IO ()
report line location message = do
  putStrLn $ "[line " ++ show line ++ "] Error" ++ location
             ++ ": " ++ message
  writeGlobal hadError True

runtimeError :: RuntimeError -> IO ()
runtimeError (RuntimeError token message) = do
  hPutStrLn stderr $ message ++ "\n[line " ++ show (tokenLine token) ++ "]"
  writeGlobal hadRuntimeError True
