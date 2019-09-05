{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Scanner (scanTokens) where

import Data.Bool (bool)
import qualified Data.HashMap.Strict as M
import Data.HashMap.Strict (HashMap)
import Data.Char (isAsciiLower,isAsciiUpper)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import Control.Monad.RWS.Strict
import Data.DList
import Data.Dynamic

import Util
import Token (Token(Token))
import qualified TokenType as TT
import TokenType (TokenType)

type MS = RWS Text (DList Token,DList (Int,Text)) ScannerState

data ScannerState = ScannerState { start :: !Int, current :: !Int, line :: !Int }

scanError :: Text -> MS ()
scanError msg = get >>= \s -> tell (empty,singleton (line s,msg))

scanTokens :: Text -> (DList Token,DList (Int,Text))
scanTokens source = snd $ execRWS m source (ScannerState 0 0 1) where
  m = do whileM_ (not <$> isAtEnd) $ do
           modify (\s -> s { start = current s })
           scanToken
         l <- gets line
         tell (singleton (Token TT.Eof "" (toDyn ()) l),empty)
  
isAtEnd :: MS Bool
isAtEnd = liftM2 (>=) (gets current) (asks T.length)

advance_ :: MS ()
advance_ = modify (\s -> s { current = current s + 1 })
advance :: MS Char
advance = do
  advance_
  liftM2 T.index ask (pred . current <$> get)

addToken :: TT.TokenType -> MS ()
addToken tokType = addTokenLit tokType (toDyn ())

addTokenLit :: TT.TokenType -> Dynamic -> MS ()
addTokenLit tokType literal = do
  text <- lexeme
  l <- gets line
  tell (singleton (Token tokType text literal l),empty)

lexeme :: MS Text
lexeme = do
  source <- ask
  s <- get
  return (substr source (start s) (current s))

scanToken :: MS ()
scanToken = do
  c <- advance
  case c of '(' -> addToken TT.LeftParen
            ')' -> addToken TT.RightParen
            '{' -> addToken TT.LeftBrace
            '}' -> addToken TT.RightBrace
            ',' -> addToken TT.Comma
            '.' -> addToken TT.Dot
            '-' -> addToken TT.Minus
            '+' -> addToken TT.Plus
            ';' -> addToken TT.Semicolon
            '*' -> addToken TT.Star
            '!' -> addToken . bool TT.Bang    TT.BangEqual    =<< match '='
            '=' -> addToken . bool TT.Equal   TT.EqualEqual   =<< match '='
            '<' -> addToken . bool TT.Less    TT.LessEqual    =<< match '='
            '>' -> addToken . bool TT.Greater TT.GreaterEqual =<< match '='
            '/' -> do
              sl <- match '/'
              if sl then whileM_ (andM [(/= '\n') <$> peek,not <$> isAtEnd])
                           advance_
                else addToken TT.Slash
            -- Ignore whitespace.
            ' ' -> return ()
            '\r' -> return ()
            '\t' -> return ()
            '\n' -> modify (\s -> s { line = line s + 1 })

            '"' -> string

            _ | isDigit c -> number
              | isAlpha c -> identifier
              | otherwise -> scanError "Unexpected character."

identifier :: MS ()
identifier = do
  whileM_ (isAlphaNumeric <$> peek) advance_

  -- See if the identifier is a reserved word
  text <- lexeme
  addToken $ M.lookupDefault TT.Identifier text keywords

number :: MS ()
number = do
  whileM_ (isDigit <$> peek) advance_

  -- Look for a fractional part
  f <- andM [(== '.') <$> peek,isDigit <$> peekNext]
  when f $ do
    -- Consume the "."
    advance_
    whileM_ (isDigit <$> peek) advance_

  addTokenLit TT.Number . toDyn @Double . readRat =<< lexeme
    where readRat t = n where Right (n,_) = T.rational t

string :: MS ()
string = do
  whileM_ (andM [(/= '"') <$> peek,not <$> isAtEnd]) $ do
    nl <- (== '\n') <$> peek
    when nl $ modify (\s -> s { line = line s + 1 })
    advance_

  -- Unterminated string
  ifM isAtEnd (scanError "Unterminated string.") $ do
    -- The closing "
    advance_

    -- Trim the surrounding quotes
    source <- ask
    s <- get
    let value = substr source (start s + 1) (current s - 1)
    addTokenLit TT.String (toDyn value)

match :: Char -> MS Bool
match c = ifM isAtEnd (return False) $ do
            c' <- liftM2 T.index ask (gets current)
            if c' /= c then return False else do
              modify (\s -> s { current = current s + 1})
              return True

peek :: MS Char
peek = ifM isAtEnd (return '\0') (liftM2 T.index ask (gets current))

peekNext :: MS Char
peekNext = ifM (liftM2 (>=) (gets $ succ . current) (asks T.length))
             (return '\0')
             (liftM2 T.index ask (gets $ succ . current))

isAlpha :: Char -> Bool
isAlpha c = isAsciiLower c || isAsciiUpper c || c == '_'

isAlphaNumeric :: Char -> Bool
isAlphaNumeric c = isAlpha c || isDigit c
            
isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

substr :: Text -> Int -> Int -> Text
substr str beg end = T.take (end-beg) (T.drop beg str)

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
