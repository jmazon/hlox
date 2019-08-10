{-# LANGUAGE ExistentialQuantification,DeriveGeneric #-}

module Interpreter where

import Data.Char
import Data.List
import qualified Data.HashMap.Strict as H
import Data.HashMap.Strict (HashMap)
import Data.Hashable
import Data.IORef
import Control.Monad
import Control.Monad.Loops
import Control.Exception hiding (evaluate)
import System.IO.Unsafe
import System.Clock

import qualified TokenType as TT
import Token
import Expr
import RuntimeError
import Misc
import Stmt
import Value
import Environment

data Interpreter = Interpreter { interpreterEnvironment :: Environment
                               , interpreterLocals :: IORef (HashMap Expr Int)}

globals :: Environment
globals = unsafePerformIO $ do
  g <- newEnvironment
  define g "clock" (VCallable $ MkCallable $ Native 0 nativeClock)
  return g

newInterpreter :: IO Interpreter
newInterpreter = Interpreter globals <$> newIORef H.empty

interpret :: Interpreter -> [Stmt] -> IO ()
interpret i statements = do
  (mapM_ (execute i) statements) `catch`
    (\e -> runtimeError (e :: RuntimeError))

evaluate :: Interpreter -> Expr -> IO Value
evaluate _ (Literal LNull) = return VNull
evaluate _ (Literal (LNumber n)) = return (VNumber n)
evaluate _ (Literal (LBool b)) = return (VBool b)
evaluate _ (Literal (LString s)) = return (VString s)
evaluate i (Logical left operator right) = do
  l <- evaluate i left
  case (tokenType operator,isTruthy l) of
    (TT.Or,True) -> return l
    (TT.And,False) -> return l
    _ -> evaluate i right
evaluate i (Grouping expr) = evaluate i expr
evaluate i (Unary operator right) = do
  r <- evaluate i right
  case tokenType operator of
    TT.Bang -> return (VBool (not (isTruthy r)))
    TT.Minus -> checkNumberOperand operator r >>
                let (VNumber n) = r
                in return (VNumber (-n))
    _ -> return VNull
evaluate i expr@(Variable name _) = lookupVariable i name expr
evaluate i expr@(Assign name _ value) = do
  v <- evaluate i value
  distance <- H.lookup expr <$> readIORef (interpreterLocals i)
  case distance of Just d -> assignAt (interpreterEnvironment i) d name v
                   Nothing -> assign globals name v
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
evaluate i (Call callee paren arguments) = do
  c <- evaluate i callee
  as <- mapM (evaluate i) arguments
  unless (isCallable c) $
    throwIO $ RuntimeError paren "Can only call functions and classes."
  let (VCallable function) = c
  when (length arguments /= arity function) $ do
    throwIO $ RuntimeError paren $ "Expected " ++ show (arity function) ++
                                   " arguments but got " ++
                                   show (length arguments) ++ "."
  call function i as

execute :: Interpreter -> Stmt -> IO ()
execute i (Expression expr) = void $ evaluate i expr
execute i f@(Function name _ _) =
  define (interpreterEnvironment i) (tokenLexeme name)
         (VCallable $ MkCallable (LoxFunction f (interpreterEnvironment i)))
execute i (If condition thenBranch elseBranch) = do
  c <- isTruthy <$> evaluate i condition
  if c then execute i thenBranch
    else maybe (pure ()) (execute i) elseBranch
execute i (Print value) = do
  v <- evaluate i value
  putStrLn (stringify v)
execute i (Return _ value) = do
  v <- maybe (return VNull) (evaluate i) value
  throwIO (ReturnException v)
execute i (Var name initializer) = do
  value <- maybe (return VNull) (evaluate i) initializer
  define (interpreterEnvironment i) (tokenLexeme name) value
execute i (While condition body) = do
  whileM_ (isTruthy <$> evaluate i condition)
    (execute i body)
execute i b@(Block statements) = executeBlock i statements =<<
                                   childEnvironment (interpreterEnvironment i)

executeBlock i statements environment =
  forM_ statements $ execute i { interpreterEnvironment = environment }

resolveI :: Interpreter -> Expr -> Int -> IO ()
resolveI i expr depth = modifyIORef (interpreterLocals i) (H.insert expr depth)

lookupVariable :: Interpreter -> Token -> Expr -> IO Value
lookupVariable i name expr = do
  distance <- H.lookup expr <$> readIORef (interpreterLocals i)
  case distance of
    Just d -> getAt (interpreterEnvironment i) d (tokenLexeme name)
    Nothing -> get globals name

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
stringify (VCallable c) = toString c

data Native = Native { nativeArity :: Int, nativeFn :: [Value] -> IO Value }
instance Callable Native where
  arity = nativeArity
  call n _ vs = (nativeFn n) vs
  toString _ = "<native fn>"

nativeClock :: [Value] -> IO Value
nativeClock [] = do
  TimeSpec sec nsec <- getTime Monotonic
  return (VNumber $ fromIntegral sec + fromIntegral nsec / 10^9)

data LoxFunction = LoxFunction { lfunDeclaration :: Stmt, lfunClosure :: Environment }
instance Callable LoxFunction where
  arity (LoxFunction (Function _ params _) _) = length params
  call (LoxFunction (Function name params body) environment) i arguments = do
    forM_ (zip params arguments) $ \(p,a) ->
      define environment (tokenLexeme p) a
    handle (\(ReturnException v) -> return v) $ do
      executeBlock i body environment
      return VNull
  toString (LoxFunction (Function name _ _) _) = "<fn " ++ tokenLexeme name ++ ">"

newLoxFunction :: Stmt -> Environment -> LoxFunction
newLoxFunction = LoxFunction

data ReturnException = ReturnException Value
instance Exception ReturnException
instance Show ReturnException where
  show = error "Undefined Show instance for ReturnException"

instance Hashable TT.TokenType
instance Hashable Literal
instance Hashable Token
instance Hashable Unique'
instance Hashable Expr
