module Token (Token(Token),tokenType,tokenLexeme,tokenLiteral,tokenLine,Literal(..)) where

import TokenType (TokenType)

data Token = Token {
    tokenType :: !TokenType
  , tokenLexeme :: String
  , tokenLiteral :: Literal
  , tokenLine :: !Int
  }

instance Show Token where
  show (Token t lx lit _) = show t ++ " " ++ lx ++ " " ++ show lit

data Literal = LNull | LString String | LNumber Double | LBool Bool
instance Show Literal where
  show LNull = "(null)"
  show (LString s) = s
  show (LNumber n) = show n
  show (LBool tf) = show tf
