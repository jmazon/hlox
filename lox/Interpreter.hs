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
import Class
import Instance
import Function
import ReturnException
import Callable

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
evaluate i (Get object name) = do
  o <- evaluate i object
  case o of VInstance inst -> getP inst name
            _ -> throwIO (RuntimeError name "Only instances have properties.")
evaluate i (Set object name value) = do
  o <- evaluate i object
  case o of VInstance inst -> do
              v <- evaluate i value
              setP inst name v
              return v
            _ -> throwIO (RuntimeError name "Only instances have fields.")
evaluate i expr@(Super _ _ method) = do
  Just distance <- H.lookup expr <$> readIORef (interpreterLocals i)
  VCallable super <- getAt (interpreterEnvironment i) distance "super"
  let Just superclass = isClass super
  -- "this" is always one level nearer than "super"'s environment
  VInstance object <- getAt (interpreterEnvironment i) (distance - 1) "this"
  case findMethod superclass (tokenLexeme method) of
    Just m -> VCallable . MkCallable <$> bind m object
    Nothing -> throwIO (RuntimeError method ("Undefined property '" ++ tokenLexeme method ++ "'."))
evaluate i expr@(This keyword _) = lookupVariable i keyword expr
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
            | otherwise -> throwIO (RuntimeError operator "Operands must be two numbers or two strings.")
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
execute i (Class name superclass methods) = do
  sc <- sequence $ flip fmap superclass $ \sc -> do
    sc' <- evaluate i sc
    case sc' of VCallable cb | Just cs <- isClass cb -> return cs
                _ -> throwIO (RuntimeError (variableName sc) "Superclass must be a class.")
  define (interpreterEnvironment i) (tokenLexeme name) VNull
  environment <- case sc of
    Just sc -> do
      environment <- childEnvironment (interpreterEnvironment i)
      define environment "super" (VCallable $ MkCallable sc)
      return environment
    Nothing -> return (interpreterEnvironment i)
  methods <- fmap H.fromList $ forM methods $ \method -> do
    let function = newFunction method environment (tokenLexeme name == "init")
    return (tokenLexeme (functionName method),function)
  let klass = newClass (tokenLexeme name) sc methods
  assign (interpreterEnvironment i) name (VCallable $ MkCallable klass)
execute i (Expression expr) = void $ evaluate i expr
execute i f@(Function name _ _) =
  define (interpreterEnvironment i) (tokenLexeme name)
         (VCallable $ MkCallable $ newFunction f (interpreterEnvironment i) False)
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

executeBlock :: Interpreter -> [Stmt] -> Environment -> IO ()
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
stringify (VInstance i) = Class.className (instanceClass i) ++ " instance"

data Native = Native { nativeArity :: Int, nativeFn :: [Value] -> IO Value }
instance Callable Native where
  arity = nativeArity
  call n _ vs = (nativeFn n) vs
  toString _ = "<native fn>"

nativeClock :: [Value] -> IO Value
nativeClock [] = do
  TimeSpec sec nsec <- getTime Monotonic
  return (VNumber $ fromIntegral sec + fromIntegral nsec / 10^9)

instance Hashable TT.TokenType
instance Hashable Literal
instance Hashable Token
instance Hashable Unique'
instance Hashable Expr
