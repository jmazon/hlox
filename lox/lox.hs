import Control.Monad
import System.Environment
import System.Exit

import Globals
import Scanner
import Parser
import AstPrinter
import Interpreter

main :: IO ()
main = do
  interpreter <- newInterpreter
  args <- getArgs
  if length args > 1
    then do putStrLn "Usage: hlox [script]"
            exitWith $ ExitFailure 64
    else if length args == 1
         then runFile interpreter (head args)
         else runPrompt interpreter

runFile :: Interpreter -> FilePath -> IO ()
runFile interpreter path = do
  bytes <- readFile path
  run interpreter bytes

  he <- readGlobal hadError
  when he $ exitWith (ExitFailure 65)
  hre <- readGlobal hadRuntimeError
  when hre $ exitWith (ExitFailure 70)

runPrompt :: Interpreter -> IO ()
runPrompt interpreter = forever $ do
  putStr "> "
  run interpreter =<< getLine
  writeGlobal hadError False

runLex,runParse :: String -> IO ()
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

run :: Interpreter -> String -> IO ()
run interpreter source = do
  scanner <- newScanner source
  tokens <- scanTokens scanner
  parser <- newParser tokens
  expr <- parse parser
  he <- readGlobal hadError
  unless he $ interpret interpreter expr
