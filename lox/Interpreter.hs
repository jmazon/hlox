module Interpreter where

import Data.Char
import Data.List
import Control.Monad
import Control.Exception hiding (evaluate)

import qualified TokenType as TT
import Token
import Expr
import RuntimeError
import Misc
import Stmt
import Value
import Environment

data Interpreter = Interpreter { interpreterEnvironment :: Environment }

newInterpreter :: IO Interpreter
newInterpreter = Interpreter <$> newEnvironment

interpret :: Interpreter -> [Stmt] -> IO ()
interpret i statements = do
  (mapM_ (execute i) statements) `catch`
    (\e -> runtimeError (e :: RuntimeError))

evaluate :: Interpreter -> Expr -> IO Value
evaluate _ (Literal (LNumber n)) = return (VNumber n)
evaluate _ (Literal (LBool b)) = return (VBool b)
evaluate _ (Literal (LString s)) = return (VString s)
evaluate i (Grouping expr) = evaluate i expr
evaluate i (Unary operator right) = do
  r <- evaluate i right
  case tokenType operator of
    TT.Bang -> return (VBool (not (isTruthy r)))
    TT.Minus -> checkNumberOperand operator r >>
                let (VNumber n) = r
                in return (VNumber (-n))
    _ -> return VNull
evaluate i (Variable name) = get (interpreterEnvironment i) name
evaluate i (Assign name value) = do
  v <- evaluate i value
  assign (interpreterEnvironment i) name v
  return v
evaluate i (Binary left operator right) = do
  l <- evaluate i left
  r <- evaluate i right
  case tokenType operator of
    TT.Greater -> checkNumberOperands operator l r >>
                  let (VNumber vl) = l; (VNumber vr) = r
                  in return (VBool (vl > vr))
    TT.GreaterEqual -> checkNumberOperands operator l r >>
                       let (VNumber vl) = l; (VNumber vr) = r
                       in return (VBool (vl >= vr))
    TT.Less -> checkNumberOperands operator l r >>
               let (VNumber vl) = l; (VNumber vr) = r
               in return (VBool (vl < vr))
    TT.LessEqual -> checkNumberOperands operator l r >>
                    let (VNumber vl) = l; (VNumber vr) = r
                    in return (VBool (vl <= vr))

    TT.BangEqual -> return (VBool (l /= r))
    TT.EqualEqual -> return (VBool (l == r))

    TT.Plus | VNumber vl <- l, VNumber vr <- r -> return (VNumber (vl + vr))
            | VString vl <- l, VString vr <- r -> return (VString (vl ++ vr))
            | otherwise -> throwIO (RuntimeError operator "Operands must be tzo nuimbers or two strings.")
    TT.Minus -> checkNumberOperands operator l r >>
                let (VNumber vl) = l; (VNumber vr) = r
                in return (VNumber (vl - vr))
    TT.Slash -> checkNumberOperands operator l r >>
                let (VNumber vl) = l; (VNumber vr) = r
                in return (VNumber (vl / vr))
    TT.Star -> checkNumberOperands operator l r >>
               let (VNumber vl) = l; (VNumber vr) = r
               in return (VNumber (vl * vr))
    _ -> return VNull

execute :: Interpreter -> Stmt -> IO ()
execute i (Expression expr) = void $ evaluate i expr
execute i (Print value) = do
  v <- evaluate i value
  putStrLn (stringify v)
execute i (Var name initializer) = do
  value <- maybe (return VNull) (evaluate i) initializer
  define (interpreterEnvironment i) (tokenLexeme name) value
execute i (Block statements) = do
  environment <- childEnvironment (interpreterEnvironment i)
  forM_ statements $ execute i { interpreterEnvironment = environment }

checkNumberOperand :: Token -> Value -> IO ()
checkNumberOperand operator operand
  | VNumber _ <- operand = return ()
  | otherwise = throwIO (RuntimeError operator "Operand must be a number.")

checkNumberOperands :: Token -> Value -> Value -> IO ()
checkNumberOperands operator left right
  | VNumber _ <- left, VNumber _ <- right = return ()
  | otherwise = throwIO (RuntimeError operator "Operands must be numbers.")

isTruthy VNull = False
isTruthy (VBool b) = b
isTruthy _ = True

stringify :: Value -> String
stringify VNull = "nil"
stringify (VBool b) = map toLower (show b)
stringify (VNumber n) | ".0" `isSuffixOf` s = init (init s)
                      | otherwise = s
  where s = show n
stringify (VString s) = s

