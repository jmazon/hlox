import Control.Monad
import System.Environment
import System.Exit

import Globals
import Scanner

main :: IO ()
main = do
  args <- getArgs
  if length args > 1
    then do putStrLn "Usage: hlox [script]"
            exitWith $ ExitFailure 64
    else if length args == 1
         then runFile (head args)
         else runPrompt

runFile :: FilePath -> IO ()
runFile path = do
  bytes <- readFile path
  run bytes

  he <- readGlobal hadError
  when he $ exitWith (ExitFailure 65)

runPrompt :: IO ()
runPrompt = forever $ do
  putStr "> "
  run =<< getLine
  writeGlobal hadError False

run :: String -> IO ()
run source = do
  scanner <- newScanner source
  tokens <- scanTokens scanner

  mapM_ print tokens
