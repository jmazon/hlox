{-# LANGUAGE DeriveGeneric #-}
module Token where

import GHC.Generics (Generic)
import Data.Unique
import TokenType

data Token = Token {
    tokenType :: !TokenType
  , tokenLexeme :: String
  , tokenLiteral :: Literal
  , tokenLine :: !Int
  } deriving (Eq,Generic)

instance Show Token where
  show (Token t lx lit _) = show t ++ " " ++ lx ++ " " ++ show lit

data Literal = LNull | LString String | LNumber Double | LBool Bool deriving (Eq,Generic)
instance Show Literal where
  show LNull = "(null)"
  show (LString s) = s
  show (LNumber n) = show n
  show (LBool tf) = show tf

newtype Unique' = Unique' Unique deriving (Eq,Ord,Generic)
instance Show Unique' where show (Unique' u) = show (hashUnique u)
