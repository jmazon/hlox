{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
import System.Environment
import System.Exit
import System.IO
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

main :: IO ()
main = do
  args <- getArgs
  interpreter <- newInterpreter
  if length args > 1
    then do putStrLn "Usage: hlox [script]"
            exitWith $ ExitFailure 64
    else if length args == 1
         then runFile interpreter (head args)
         else runPrompt interpreter

runFile :: Interpreter -> FilePath -> IO ()
runFile interpreter path = do
  bytes <- T.readFile path
  (_,result) <- run interpreter bytes

  case result of NoError         -> exitSuccess
                 HadError        -> exitWith (ExitFailure 65)
                 HadRuntimeError -> exitWith (ExitFailure 70)

runPrompt :: Interpreter -> IO ()
runPrompt interpreter = do
  putStr "> "
  hFlush stdout
  runPrompt . fst =<< run interpreter =<< T.getLine

data RunResult = NoError | HadError | HadRuntimeError

run :: Interpreter -> Text -> IO (Interpreter,RunResult)
run interpreter source = do
  let (tokens,scanErrors) = scanTokens source
  mapM_ (uncurry scanError) scanErrors
  let (statements,parseErrors) = parse tokens
  mapM_ (uncurry tokenError) parseErrors
  if not (null scanErrors && null parseErrors)
    then return (interpreter,HadError)
    else do
      let (locals,resolveErrors) = resolve statements
      mapM_ (uncurry tokenError) resolveErrors
      let i' = resolveLocals interpreter locals
      if not (null resolveErrors) then return (i',HadError)
        else fmap (i',) . maybe (return NoError) runtimeError
               =<< interpret i' statements

scanError :: Int -> Text -> IO RunResult
scanError line message = report line "" message
{-# ANN scanError ("HLint: ignore Eta reduce" :: String) #-}

report :: Int -> Text -> Text -> IO RunResult
report line location message = do
  T.hPutStrLn stderr $ T.concat [ "[line ", T.pack (show line)
                                , "] Error" ,location
                                , ": ", message ]
  return HadError

tokenError :: Token -> Text -> IO RunResult
tokenError t m =
  if tokenType t == TT.Eof
    then report (tokenLine t) " at end" m
    else report (tokenLine t) (T.concat [" at '",tokenLexeme t,"'"]) m

runtimeError :: RuntimeError -> IO RunResult
runtimeError (RuntimeError token message) = do
  T.hPutStrLn stderr $ T.concat [ message
                                , "\n[line "
                                , T.pack (show (tokenLine token))
                                , "]" ]
  return HadRuntimeError
