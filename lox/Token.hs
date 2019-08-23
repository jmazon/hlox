module Token (Token(Token),tokenType,tokenLexeme,tokenLiteral,tokenLine,Literal(..)) where

import Data.Text (Text)
import qualified Data.Text as T

import TokenType (TokenType)

data Token = Token {
    tokenType :: !TokenType
  , tokenLexeme :: Text
  , tokenLiteral :: Literal
  , tokenLine :: !Int
  }

instance Show Token where
  show (Token t lx lit _) = show t ++ " " ++ T.unpack lx ++ " " ++ show lit

data Literal = LNull | LString Text | LNumber Double | LBool Bool
instance Show Literal where
  show LNull = "(null)"
  show (LString s) = T.unpack s
  show (LNumber n) = show n
  show (LBool tf) = show tf
