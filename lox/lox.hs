{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import System.Environment
import System.Exit
import System.IO
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

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
  bytes <- T.readFile path
  run lox interpreter bytes

  he <- readIORef (hadError lox)
  when he $ exitWith (ExitFailure 65)
  hre <- readIORef (hadRuntimeError lox)
  when hre $ exitWith (ExitFailure 70)

runPrompt :: Lox -> Interpreter -> IO ()
runPrompt lox interpreter = forever $ do
  putStr "> "
  hFlush stdout
  run lox interpreter =<< T.getLine
  writeIORef (hadError lox) False

run :: Lox -> Interpreter -> Text -> IO ()
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

scanError :: Lox -> Int -> Text -> IO ()
scanError lox line message = report lox line "" message
{-# ANN scanError ("HLint: ignore Eta reduce" :: String) #-}

report :: Lox -> Int -> Text -> Text -> IO ()
report lox line location message = do
  T.hPutStrLn stderr $ T.concat [ "[line ", T.pack (show line)
                                , "] Error" ,location
                                , ": ", message ]
  writeIORef (hadError lox) True

tokenError :: Lox -> Token -> Text -> IO ()
tokenError lox t m =
  if tokenType t == TT.Eof
    then report lox (tokenLine t) " at end" m
    else report lox (tokenLine t) (T.concat [" at '",tokenLexeme t,"'"]) m

runtimeError :: Lox -> RuntimeError -> IO ()
runtimeError lox (RuntimeError token message) = do
  T.hPutStrLn stderr $ T.concat [ message
                                , "\n[line "
                                , T.pack (show (tokenLine token))
                                , "]" ]
  writeIORef (hadRuntimeError lox) True
