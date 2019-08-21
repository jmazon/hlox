import Control.Monad
import System.Environment
import System.Exit
import System.IO
import Data.IORef

import Util
import Scanner (newScanner,scanTokens)
import Parser (newParser,parse)
import Interpreter (Interpreter,newInterpreter,interpret,resolveLocals)
import Resolver (newResolver,resolve)
import Token (Token,tokenType,tokenLine,tokenLexeme)
import qualified TokenType as TT (TokenType(Eof))
import RuntimeError (RuntimeError(RuntimeError))

data Lox = Lox { hadError :: IORef Bool
               , hadRuntimeError :: IORef Bool }

newLox :: IO Lox
newLox = liftM2 Lox (newIORef False) (newIORef False)

main :: IO ()
main = do
  lox <- newLox
  interpreter <- newInterpreter
  args <- getArgs
  if length args > 1
    then do putStrLn "Usage: hlox [script]"
            exitWith $ ExitFailure 64
    else if length args == 1
         then runFile lox interpreter (head args)
         else runPrompt lox interpreter

runFile :: Lox -> Interpreter -> FilePath -> IO ()
runFile lox interpreter path = do
  bytes <- readFile path
  run lox interpreter bytes

  he <- readIORef (hadError lox)
  when he $ exitWith (ExitFailure 65)
  hre <- readIORef (hadRuntimeError lox)
  when hre $ exitWith (ExitFailure 70)

runPrompt :: Lox -> Interpreter -> IO ()
runPrompt lox interpreter = forever $ do
  putStr "> "
  hFlush stdout
  run lox interpreter =<< getLine
  writeIORef (hadError lox) False

run :: Lox -> Interpreter -> String -> IO ()
run lox interpreter source = do
  scanner <- newScanner (scanError lox) source
  tokens <- scanTokens scanner
  parser <- newParser (tokenError lox) tokens
  statements <- parse parser
  unlessM (readIORef (hadError lox)) $ do
    resolver <- newResolver (tokenError lox)
    resolveLocals interpreter =<< resolve resolver statements
    unlessM (readIORef (hadError lox)) $
      interpret (runtimeError lox) interpreter statements

scanError :: Lox -> Int -> String -> IO ()
scanError lox line message = report lox line "" message
{-# ANN scanError "HLint: ignore Eta reduce" #-}

report :: Lox -> Int -> String -> String -> IO ()
report lox line location message = do
  hPutStrLn stderr $ "[line " ++ show line ++ "] Error" ++ location
                     ++ ": " ++ message
  writeIORef (hadError lox) True

tokenError :: Lox -> Token -> String -> IO ()
tokenError lox t m =
  if tokenType t == TT.Eof
    then report lox (tokenLine t) " at end" m
    else report lox (tokenLine t) (" at '" ++ tokenLexeme t ++ "'") m

runtimeError :: Lox -> RuntimeError -> IO ()
runtimeError lox (RuntimeError token message) = do
  hPutStrLn stderr $ message ++ "\n[line " ++ show (tokenLine token) ++ "]"
  writeIORef (hadRuntimeError lox) True
