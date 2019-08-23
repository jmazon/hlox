{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import System.Environment
import System.Exit
import System.IO
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Scanner (scanTokens)
import Parser (parse)
import Interpreter (Interpreter,newInterpreter,interpret,resolveLocals)
import Resolver (resolve)
import Token (Token,tokenType,tokenLine,tokenLexeme)
import qualified TokenType as TT (TokenType(Eof))
import RuntimeError (RuntimeError(RuntimeError))

data Lox = Lox { interpreter :: Interpreter
               , hadError :: IORef Bool }

newLox :: Interpreter -> IO Lox
newLox i = fmap (Lox i) (newIORef False)

main :: IO ()
main = do
  lox <- newLox =<< newInterpreter
  args <- getArgs
  if length args > 1
    then do putStrLn "Usage: hlox [script]"
            exitWith $ ExitFailure 64
    else if length args == 1
         then runFile lox (head args)
         else runPrompt lox

runFile :: Lox -> FilePath -> IO ()
runFile lox path = do
  bytes <- T.readFile path
  result <- run lox bytes

  case result of
    NoError -> return ()
    HadError -> exitWith (ExitFailure 65)
    HadRuntimeError -> exitWith (ExitFailure 70)

runPrompt :: Lox -> IO ()
runPrompt lox = forever $ do
  putStr "> "
  hFlush stdout
  run lox =<< T.getLine
  writeIORef (hadError lox) False

data RunResult = NoError | HadError | HadRuntimeError

run :: Lox -> Text -> IO RunResult
run lox source = do
  tokens <- scanTokens (scanError lox) source
  statements <- parse (tokenError lox) tokens
  he1 <- readIORef (hadError lox)
  if he1 then return HadError else do
    resolveLocals (interpreter lox) =<< resolve (tokenError lox) statements
    he2 <- readIORef (hadError lox)
    if he2 then return HadError else do
      result <- interpret (interpreter lox) statements
      case result of
        Left rte -> runtimeError rte
        _ -> return NoError

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

runtimeError :: RuntimeError -> IO RunResult
runtimeError (RuntimeError token message) = do
  T.hPutStrLn stderr $ T.concat [ message
                                , "\n[line "
                                , T.pack (show (tokenLine token))
                                , "]" ]
  return HadRuntimeError
