{-# LANGUAGE OverloadedStrings #-}
module AstPrinter (printAst) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Expr
import Token
import TokenType

printAst :: Expr -> Text
printAst (Binary left op right) = parenthesize (tokenLexeme op) [left,right]
printAst (Grouping expr) = parenthesize "group" [expr]
printAst (Literal value) = T.pack (show value)
printAst (Unary op right) = parenthesize (tokenLexeme op) [right]

parenthesize :: Text -> [Expr] -> Text
parenthesize name exprs = T.concat ["(",T.unwords (name : map printAst exprs),")"]

main :: IO ()
main = do
  let expr = Binary (Unary (Token Minus "-" LNull 1) (Literal $ LNumber 123))
                    (Token Star "*" LNull 1)
                    (Grouping (Literal $ LNumber 45.67))
  T.putStrLn $ printAst expr
