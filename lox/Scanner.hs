{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Scanner (scanTokens) where

import Data.Bool
import Data.IORef
import Control.Monad
import qualified Data.HashMap.Strict as M
import Data.HashMap.Strict (HashMap)
import Data.Char (isAsciiLower,isAsciiUpper)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import Control.Monad.Writer
import Data.DList

import Util
import Token (Token(Token),Literal(LNull,LNumber,LString))
import qualified TokenType as TT
import TokenType (TokenType)

data Scanner = Scanner {
    scannerSource :: Text
  , scannerTokens :: IORef [Token]
  , scannerStart :: IORef Int
  , scannerCurrent :: IORef Int
  , scannerLine :: IORef Int
  }

type ScanError = (Int,Text)

scanError :: MonadWriter (DList ScanError) m => Int -> Text -> m ()
scanError line msg = tell (singleton (line,msg))

newScanner :: Text -> IO Scanner
newScanner source = liftM4 (Scanner source)
                      (newIORef [])
                      (newIORef 0)
                      (newIORef 0)
                      (newIORef 1)

scanTokens :: Text -> IO ([Token],DList ScanError)
scanTokens source = runWriterT $ do
  s <- liftIO $ newScanner source
  whileM_ (liftIO $ not <$> isAtEnd s) $ do
    liftIO $ writeIORef (scannerStart s) =<< readIORef (scannerCurrent s)
    scanToken s
  l <- liftIO $ readIORef (scannerLine s)
  liftIO $ modifyIORef (scannerTokens s) (++ [Token TT.Eof "" LNull l])
  liftIO $ readIORef (scannerTokens s)
  
isAtEnd :: Scanner -> IO Bool
isAtEnd s = (>= T.length (scannerSource s)) <$> readIORef (scannerCurrent s)

advance_ :: Scanner -> IO ()
advance_ s = modifyIORef (scannerCurrent s) succ
advance :: Scanner -> IO Char
advance s = do
  advance_ s
  T.index (scannerSource s) . pred <$> readIORef (scannerCurrent s)

addToken :: Scanner -> TT.TokenType -> IO ()
addToken s tokType = addTokenLit s tokType LNull

addTokenLit :: Scanner -> TT.TokenType -> Literal -> IO ()
addTokenLit s tokType literal = do
  text <- liftM2 (substr (scannerSource s))
            (readIORef (scannerStart s))
            (readIORef (scannerCurrent s))
  l <- readIORef (scannerLine s)
  modifyIORef (scannerTokens s) (++ [Token tokType text literal l])
  
scanToken :: (MonadWriter (DList ScanError) m,MonadIO m) => Scanner -> m ()
scanToken s = do
  c <- liftIO $ advance s
  case c of '(' -> liftIO $ addToken s TT.LeftParen
            ')' -> liftIO $ addToken s TT.RightParen
            '{' -> liftIO $ addToken s TT.LeftBrace
            '}' -> liftIO $ addToken s TT.RightBrace
            ',' -> liftIO $ addToken s TT.Comma
            '.' -> liftIO $ addToken s TT.Dot
            '-' -> liftIO $ addToken s TT.Minus
            '+' -> liftIO $ addToken s TT.Plus
            ';' -> liftIO $ addToken s TT.Semicolon
            '*' -> liftIO $ addToken s TT.Star
            '!' -> liftIO $ addToken s . bool TT.Bang TT.BangEqual =<< match s '='
            '=' -> liftIO $ addToken s . bool TT.Equal TT.EqualEqual =<< match s '='
            '<' -> liftIO $ addToken s . bool TT.Less TT.LessEqual =<< match s '='
            '>' -> liftIO $ addToken s . bool TT.Greater TT.GreaterEqual =<< match s '='
            '/' -> do
              sl <- liftIO $ match s '/'
              if sl then whileM_ (liftIO $ andM [(/= '\n') <$> peek s,
                                                 not <$> isAtEnd s])
                           (liftIO $ advance_ s)
                else liftIO $ addToken s TT.Slash
            -- Ignore whitespace.
            ' ' -> return ()
            '\r' -> return ()
            '\t' -> return ()
            '\n' -> liftIO $ modifyIORef (scannerLine s) succ

            '"' -> string s

            _ | isDigit c -> liftIO $ number s
              | isAlpha c -> liftIO $ identifier s
              | otherwise -> liftIO (readIORef (scannerLine s)) >>= \l ->
                             scanError l "Unexpected character."

identifier :: Scanner -> IO ()
identifier s = do
  whileM_ (isAlphaNumeric <$> peek s) (advance_ s)

  -- See if the identifier is a reserved word
  text <- liftM2 (substr (scannerSource s))
            (readIORef (scannerStart s))
            (readIORef (scannerCurrent s))
  addToken s $ M.lookupDefault TT.Identifier text keywords

number :: Scanner -> IO ()
number s = do
  whileM_ (isDigit <$> peek s) (advance_ s)

  -- Look for a fractional part
  f <- andM [ (== '.') <$> peek s
            , isDigit <$> peekNext s ]
  when f $ do
    -- Consume the "."
    advance_ s
    whileM_ (isDigit <$> peek s) (advance_ s)

  addTokenLit s TT.Number . LNumber . readRat =<<
    liftM2 (substr (scannerSource s))
      (readIORef (scannerStart s))
      (readIORef (scannerCurrent s))
    where readRat t = n where Right (n,_) = T.rational t

string :: (MonadWriter (DList ScanError) m,MonadIO m) => Scanner -> m ()
string s = do
  liftIO $ whileM_ (andM [(/= '"') <$> peek s
                , not <$> isAtEnd s]) $ do
    nl <- (== '\n') <$> peek s
    when nl $ modifyIORef (scannerLine s) succ
    advance_ s

  -- Unterminated string
  ae <- liftIO $ isAtEnd s
  if ae then do
    l <- liftIO $ readIORef (scannerLine s)
    scanError l "Unterminated string."
      else do

    -- The closing "
    liftIO $ advance_ s

    -- Trim the surrounding quotes
    value <- liftIO $  liftM2 (substr (scannerSource s))
                        (succ <$> readIORef (scannerStart s))
                        (pred <$> readIORef (scannerCurrent s))
    liftIO $ addTokenLit s TT.String (LString value)

match :: Scanner -> Char -> IO Bool
match s c = do
  ae <- isAtEnd s
  if ae then return False else do
    cur <- readIORef (scannerCurrent s)
    let c' = scannerSource s `T.index` cur
    if c' /= c then return False else do
      modifyIORef (scannerCurrent s) succ
      return True

peek :: Scanner -> IO Char
peek s = do
  ae <- isAtEnd s
  if ae
    then return '\0'
    else T.index (scannerSource s) <$> readIORef (scannerCurrent s)

peekNext :: Scanner -> IO Char
peekNext s = do
  c <- readIORef (scannerCurrent s)
  if c + 1 >= T.length (scannerSource s)
    then return '\0'
    else return (scannerSource s `T.index` (c + 1))

isAlpha :: Char -> Bool
isAlpha c = isAsciiLower c || isAsciiUpper c || c == '_'

isAlphaNumeric :: Char -> Bool
isAlphaNumeric c = isAlpha c || isDigit c
            
isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

substr :: Text -> Int -> Int -> Text
substr str start end = T.take (end-start) (T.drop start str)

keywords :: HashMap Text TokenType
keywords = M.fromList [ ("and",    TT.And)
                      , ("class",  TT.Class)
                      , ("else",   TT.Else)
                      , ("false",  TT.False)
                      , ("for",    TT.For)
                      , ("fun",    TT.Fun)
                      , ("if",     TT.If)
                      , ("nil",    TT.Nil)
                      , ("or",     TT.Or)
                      , ("print",  TT.Print)
                      , ("return", TT.Return)
                      , ("super",  TT.Super)
                      , ("this",   TT.This)
                      , ("true",   TT.True)
                      , ("var",    TT.Var)
                      , ("while",  TT.While) ]
