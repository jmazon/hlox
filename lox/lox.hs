import Control.Monad
import System.Environment
import System.Exit

import Globals
import Scanner
import Parser
import AstPrinter
import Interpreter
import Resolver

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

run :: Interpreter -> String -> IO ()
run interpreter source = do
  scanner <- newScanner source
  tokens <- scanTokens scanner
  parser <- newParser tokens
  statements <- parse parser
  he <- readGlobal hadError
  unless he $ do
    resolver <- newResolver interpreter
    mapM (resolveS resolver) statements
    he <- readGlobal hadError
    unless he $ interpret interpreter statements
