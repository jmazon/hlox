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

import Util
import Token (Token(Token),Literal(LNull,LNumber,LString))
import qualified TokenType as TT
import TokenType (TokenType)

data Scanner = Scanner {
    scannerError :: Int -> Text -> IO ()
  , scannerSource :: Text
  , scannerTokens :: IORef [Token]
  , scannerStart :: IORef Int
  , scannerCurrent :: IORef Int
  , scannerLine :: IORef Int
  }

newScanner :: (Int -> Text -> IO ()) -> Text -> IO Scanner
newScanner err source = liftM4 (Scanner err source)
                          (newIORef [])
                          (newIORef 0)
                          (newIORef 0)
                          (newIORef 1)

scanTokens :: (Int -> Text -> IO ()) -> Text -> IO [Token]
scanTokens err source = do
  s <- newScanner err source
  whileM_ (not <$> isAtEnd s) $ do
    writeIORef (scannerStart s) =<< readIORef (scannerCurrent s)
    scanToken s
  l <- readIORef (scannerLine s)
  modifyIORef (scannerTokens s) (++ [Token TT.Eof "" LNull l])
  readIORef (scannerTokens s)
  
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
  
scanToken :: Scanner -> IO ()
scanToken s = do
  c <- advance s
  case c of '(' -> addToken s TT.LeftParen
            ')' -> addToken s TT.RightParen
            '{' -> addToken s TT.LeftBrace
            '}' -> addToken s TT.RightBrace
            ',' -> addToken s TT.Comma
            '.' -> addToken s TT.Dot
            '-' -> addToken s TT.Minus
            '+' -> addToken s TT.Plus
            ';' -> addToken s TT.Semicolon
            '*' -> addToken s TT.Star
            '!' -> addToken s . bool TT.Bang TT.BangEqual =<< match s '='
            '=' -> addToken s . bool TT.Equal TT.EqualEqual =<< match s '='
            '<' -> addToken s . bool TT.Less TT.LessEqual =<< match s '='
            '>' -> addToken s . bool TT.Greater TT.GreaterEqual =<< match s '='
            '/' -> do
              sl <- match s '/'
              if sl then whileM_ (andM [(/= '\n') <$> peek s,
                                        not <$> isAtEnd s])
                           (advance_ s)
                else addToken s TT.Slash
            -- Ignore whitespace.
            ' ' -> return ()
            '\r' -> return ()
            '\t' -> return ()
            '\n' -> modifyIORef (scannerLine s) succ

            '"' -> string s

            _ | isDigit c -> number s
              | isAlpha c -> identifier s
              | otherwise -> readIORef (scannerLine s) >>= \l ->
                             scannerError s l "Unexpected character."

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

string :: Scanner ->  IO ()
string s = do
  whileM_ (andM [(/= '"') <$> peek s
                , not <$> isAtEnd s]) $ do
    nl <- (== '\n') <$> peek s
    when nl $ modifyIORef (scannerLine s) succ
    advance_ s

  -- Unterminated string
  ae <- isAtEnd s
  if ae then do
    l <- readIORef (scannerLine s)
    scannerError s l "Unterminated string."
      else do

    -- The closing "
    advance_ s

    -- Trim the surrounding quotes
    value <- liftM2 (substr (scannerSource s))
               (succ <$> readIORef (scannerStart s))
               (pred <$> readIORef (scannerCurrent s))
    addTokenLit s TT.String (LString value)

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
