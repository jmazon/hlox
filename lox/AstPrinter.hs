module AstPrinter where

import Expr
import Token
import TokenType

printAst :: Expr -> String
printAst (Binary left op right) = parenthesize (tokenLexeme op) [left,right]
printAst (Grouping expr) = parenthesize "group" [expr]
printAst (Literal value) = show value
printAst (Unary op right) = parenthesize (tokenLexeme op) [right]

parenthesize :: String -> [Expr] -> String
parenthesize name exprs = "(" ++ unwords (name : map printAst exprs) ++ ")"

main :: IO ()
main = do
  let expr = Binary (Unary (Token Minus "-" LNull 1) (Literal $ LNumber 123))
                    (Token Star "*" LNull 1)
                    (Grouping (Literal $ LNumber 45.67))
  putStrLn $ printAst expr
