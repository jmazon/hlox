import Control.Monad
import System.Environment
import System.Exit

import Globals
import Scanner
import Parser
import AstPrinter

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

run,runLex,runParse :: String -> IO ()
run = runParse
runLex source = do
  scanner <- newScanner source
  tokens <- scanTokens scanner
  mapM_ print tokens
runParse source = do
  scanner <- newScanner source
  tokens <- scanTokens scanner
  parser <- newParser tokens
  expr <- parse parser
  he <- readGlobal hadError
  unless he $ do
    putStrLn $ printAst expr
