module Interpreter where

import Data.List
import Control.Exception hiding (evaluate)

import qualified TokenType as TT
import Token
import Expr
import RuntimeError
import Misc

data Interpreter = Interpreter

newInterpreter :: IO Interpreter
newInterpreter = return Interpreter

interpret :: Interpreter -> Expr -> IO ()
interpret _ expr = do
  (evaluate expr >>= putStrLn . stringify) `catch`
    (\e -> runtimeError (e :: RuntimeError))

data Value = VNull | VNumber Double | VBool Bool | VString String deriving Eq

evaluate (Literal (LNumber n)) = return (VNumber n)
evaluate (Grouping expr) = evaluate expr
evaluate (Unary operator right) = do
  r <- evaluate right
  case tokenType operator of
    TT.Bang -> return (VBool (not (isTruthy r)))
    TT.Minus -> checkNumberOperand operator r >>
                let (VNumber n) = r
                in return (VNumber (-n))
    _ -> return VNull
evaluate (Binary left operator right) = do
  l <- evaluate left
  r <- evaluate right
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
stringify (VNumber n) | ".0" `isSuffixOf` s = init (init s)
                      | otherwise = s
  where s = show n
stringify _ = error $ "stringify not fully implemented."
