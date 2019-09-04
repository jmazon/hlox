module Token (Token(Token),tokenType,tokenLexeme,tokenLiteral,tokenLine) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Dynamic

import TokenType (TokenType)

data Token = Token {
    tokenType :: !TokenType
  , tokenLexeme :: Text
  , tokenLiteral :: Dynamic
  , tokenLine :: !Int
  }

instance Show Token where
  show (Token t lx lit _) = show t ++ " " ++ T.unpack lx ++ " " ++ show lit
