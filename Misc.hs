module Misc where

import Globals

loxError :: Int -> String -> IO ()
loxError line message = do
  report line "" message

report :: Int -> String -> String -> IO ()
report line location message = do
  putStrLn $ "[line " ++ show line ++ "] Error" ++ location
             ++ ": " ++ message
  writeGlobal hadError True
