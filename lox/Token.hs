module Token where

import TokenType

data Token = Token {
    tokenType :: !TokenType
  , tokenLexeme :: String
  , tokenLiteral :: Literal
  , tokenLine :: !Int
  }

instance Show Token where
  show (Token t lex lit _) = show t ++ " " ++ lex ++ " " ++ show lit

data Literal = LNull | LString String | LNumber Double
instance Show Literal where
  show LNull = "(null)"
  show (LString s) = s
  show (LNumber n) = show n
