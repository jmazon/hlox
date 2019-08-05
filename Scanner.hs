module Scanner where

import Data.Bool
import Data.IORef
import Control.Monad
import Control.Monad.Loops
import qualified Data.HashMap.Strict as M
import Data.HashMap.Strict (HashMap)

import Token
import qualified TokenType as TT
import TokenType (TokenType)
import Misc

data Scanner = Scanner {
    scannerSource :: String
  , scannerTokens :: IORef [Token]
  , scannerStart :: IORef Int
  , scannerCurrent :: IORef Int
  , scannerLine :: IORef Int
  }

newScanner :: String -> IO Scanner
newScanner source = liftM4 (Scanner source)
                      (newIORef [])
                      (newIORef 0)
                      (newIORef 0)
                      (newIORef 1)

scanTokens :: Scanner -> IO [Token]
scanTokens s = do
  while (not <$> isAtEnd s) $ do
    writeIORef (scannerStart s) =<< readIORef (scannerCurrent s)
    scanToken s
  l <- readIORef (scannerLine s)
  modifyIORef (scannerTokens s) (++ [Token TT.Eof "" LNull l])
  readIORef (scannerTokens s)
  
isAtEnd :: Scanner -> IO Bool
isAtEnd s = (>= length (scannerSource s)) <$>readIORef (scannerCurrent s)

advance :: Scanner -> IO Char
advance s = do
  modifyIORef (scannerCurrent s) succ
  (scannerSource s !!) . pred <$> readIORef (scannerCurrent s)

addToken :: Scanner -> TT.TokenType -> IO ()
addToken s tokenType = addTokenLit s tokenType LNull

addTokenLit :: Scanner -> TT.TokenType -> Literal -> IO ()
addTokenLit s tokenType literal = do
  text <- liftM2 (substr (scannerSource s))
            (readIORef (scannerStart s))
            (readIORef (scannerCurrent s))
  l <- readIORef (scannerLine s)
  modifyIORef (scannerTokens s) (++ [Token tokenType text literal l])
  
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
              if sl then while (andM [(/= '\n') <$> peek s,
                                      not <$> isAtEnd s])
                           (advance s)
                else addToken s TT.Slash
            -- Ignore whitespace.
            ' ' -> return ()
            '\r' -> return ()
            '\t' -> return ()
            '\n' -> modifyIORef (scannerLine s) succ

            '"' -> string s

            _ -> do
              if isDigit c then number s
                else if isAlpha c then identifier s else
                readIORef (scannerLine s) >>= \l ->
                loxError l "Unexpected character."

identifier :: Scanner -> IO ()
identifier s = do
  while (isAlphaNumeric <$> peek s) (advance s)

  -- See if the identifier is a reserved word
  text <- liftM2 (substr (scannerSource s))
            (readIORef (scannerStart s))
            (readIORef (scannerCurrent s))
  addToken s $ M.lookupDefault TT.Identifier text keywords

number :: Scanner -> IO ()
number s = do
  while (isDigit <$> peek s) (advance s)

  -- Look for a fractional part
  f <- andM [ (== '.') <$> peek s
            , isDigit <$> peekNext s ]
  when f $ do
    -- Consume the "."
    advance s
    while (isDigit <$> peek s) (advance s)

  addTokenLit s TT.Number . LNumber . read =<<
    liftM2 (substr (scannerSource s))
      (readIORef (scannerStart s))
      (readIORef (scannerCurrent s))

string :: Scanner -> IO ()
string s = do
  while (andM [(/= '"') <$> peek s
              , not <$> isAtEnd s]) $ do
    nl <- (== '\n') <$> peek s
    when nl $ modifyIORef (scannerLine s) succ
    advance s

  -- Unterminated string
  ae <- isAtEnd s
  if ae then do
    l <- readIORef (scannerLine s)
    loxError l "Unterminated string"
      else do

    -- The closing "
    advance s

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
    let c' = scannerSource s !! cur
    if c' /= c then return False else do
      modifyIORef (scannerCurrent s) succ
      return True

peek :: Scanner -> IO Char
peek s = do
  ae <- isAtEnd s
  if ae
    then return '\0'
    else (scannerSource s !!) <$> readIORef (scannerCurrent s)

peekNext :: Scanner -> IO Char
peekNext s = do
  c <- readIORef (scannerCurrent s)
  if (c + 1 >= length (scannerSource s))
    then return '\0'
    else return (scannerSource s !! (c + 1))

isAlpha :: Char -> Bool
isAlpha c = c >= 'a' && c <= 'z' ||
            c >= 'A' && c <= 'Z' ||
            c == '_'

isAlphaNumeric :: Char -> Bool
isAlphaNumeric c = isAlpha c || isDigit c
            
isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

while :: Monad m => m Bool -> m a -> m ()
while c a = go where
  go = do b <- c
          when b (a >> go)

substr :: String -> Int -> Int -> String
substr str start end = take (end-start) (drop start str)

keywords :: HashMap String TokenType
keywords = M.fromList [ ("and", TT.And)
                      , ("class", TT.Class)
                      , ("else", TT.Else)
                      , ("false", TT.False)
                      , ("for", TT.For)
                      , ("fun", TT.Fun)
                      , ("if", TT.If)
                      , ("nil", TT.Nil)
                      , ("or", TT.Or)
                      , ("print", TT.Print)
                      , ("return", TT.Return)
                      , ("super", TT.Super)
                      , ("this", TT.This)
                      , ("true", TT.True)
                      , ("var", TT.Var)
                      , ("while", TT.While) ]
