{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Interpreter where

import Data.Char (toLower)
import Data.List (isSuffixOf)
import qualified Data.HashMap.Strict as H
import Data.HashMap.Strict (HashMap)
import Data.Hashable
import Data.IORef
import Control.Monad
import Control.Monad.Loops
import Control.Exception hiding (evaluate)
import System.IO.Unsafe
import System.Clock
import Numeric
import Data.Unique
import Data.Dynamic
import Control.Applicative

import qualified TokenType as TT
import Token
import Expr
import RuntimeError
import Misc
import Stmt
import Environment
import LoxClass
import LoxInstance
import LoxFunction
import Return
import LoxCallable

data Interpreter = Interpreter { interpreterEnvironment :: Environment
                               , interpreterLocals :: IORef (HashMap Expr Int)}

globals :: Environment
globals = unsafePerformIO $ do
  g <- newEnvironment
  define g "clock" . toDyn . Native 0 nativeClock =<< newUnique
  return g

newInterpreter :: IO Interpreter
newInterpreter = Interpreter globals <$> newIORef H.empty

interpret :: Interpreter -> [Stmt] -> IO ()
interpret i statements = do
  (mapM_ (execute i) statements) `catch`
    (\e -> runtimeError (e :: RuntimeError))

evaluate :: Interpreter -> Expr -> IO Dynamic
evaluate _ (Literal LNull) = return (toDyn ())
evaluate _ (Literal (LNumber n)) = return (toDyn n)
evaluate _ (Literal (LBool b)) = return (toDyn b)
evaluate _ (Literal (LString s)) = return (toDyn s)
evaluate i (Logical left operator right) = do
  l <- evaluate i left
  case (tokenType operator,isTruthy l) of
    (TT.Or,True) -> return l
    (TT.And,False) -> return l
    _ -> evaluate i right
evaluate i (Get object name) = do
  o <- evaluate i object
  case fromDynamic o of Just inst -> getP inst name
                        Nothing -> throwIO (RuntimeError name "Only instances have properties.")
evaluate i (Set object name value) = do
  o <- evaluate i object
  case fromDynamic o of Just inst -> do
                          v <- evaluate i value
                          setP inst name v
                          return v
                        Nothing -> throwIO (RuntimeError name "Only instances have fields.")
evaluate i expr@(Super _ _ method) = do
  Just distance <- H.lookup expr <$> readIORef (interpreterLocals i)
  Just superclass <- fromDynamic <$> getAt (interpreterEnvironment i) distance "super"
  -- "this" is always one level nearer than "super"'s environment
  Just object <- fromDynamic <$> getAt (interpreterEnvironment i) (distance - 1) "this"
  case findMethod superclass (tokenLexeme method) of
    Just m -> toDyn <$> bind m object
    Nothing -> throwIO (RuntimeError method ("Undefined property '" ++ tokenLexeme method ++ "'."))
evaluate i expr@(This keyword _) = lookupVariable i keyword expr
evaluate i (Grouping expr) = evaluate i expr
evaluate i (Unary operator right) = do
  r <- evaluate i right
  case tokenType operator of
    TT.Bang -> return (toDyn (not (isTruthy r)))
    TT.Minus -> checkNumberOperand operator r >>
                let Just (n :: Double) = fromDynamic r
                in return (toDyn (-n))
    _ -> return (toDyn ())
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
                  let Just (vl :: Double) = fromDynamic l; Just vr = fromDynamic r
                  in return (toDyn (vl > vr))
    TT.GreaterEqual -> checkNumberOperands operator l r >>
                       let Just (vl :: Double) = fromDynamic l; Just vr = fromDynamic r
                       in return (toDyn (vl >= vr))
    TT.Less -> checkNumberOperands operator l r >>
               let Just (vl :: Double) = fromDynamic l; Just vr = fromDynamic r
               in return (toDyn (vl < vr))
    TT.LessEqual -> checkNumberOperands operator l r >>
                    let Just (vl :: Double) = fromDynamic l; Just vr = fromDynamic r
                    in return (toDyn (vl <= vr))

    TT.BangEqual -> return (toDyn (not (isEqual l r)))
    TT.EqualEqual -> return (toDyn (isEqual l r))

    TT.Plus | Just vl <- fromDynamic l, Just vr <- fromDynamic r -> return (toDyn (vl + vr :: Double))
            | Just vl <- fromDynamic l, Just vr <- fromDynamic r -> return (toDyn (vl ++ vr :: String))
            | otherwise -> throwIO (RuntimeError operator "Operands must be two numbers or two strings.")
    TT.Minus -> checkNumberOperands operator l r >>
                let (Just vl) = fromDynamic l; (Just vr) = fromDynamic r
                in return (toDyn (vl - vr :: Double))
    TT.Slash -> checkNumberOperands operator l r >>
                let (Just vl) = fromDynamic l; (Just vr) = fromDynamic r
                in return (toDyn (vl / vr :: Double))
    TT.Star -> checkNumberOperands operator l r >>
               let (Just vl) = fromDynamic l; (Just vr) = fromDynamic r
               in return (toDyn (vl * vr :: Double))
    _ -> return (toDyn ())
evaluate i (Call callee paren arguments) = do
  c <- evaluate i callee
  as <- mapM (evaluate i) arguments
  case dynToCallable c of
    Nothing -> throwIO $ RuntimeError paren "Can only call functions and classes."
    Just (IsCallable function) -> do
      when (length arguments /= arity function) $ do
        throwIO $ RuntimeError paren $ "Expected " ++ show (arity function) ++
                                       " arguments but got " ++
                                       show (length arguments) ++ "."
      call function i as

execute :: Interpreter -> Stmt -> IO ()
execute i (Class name superclass methods) = do
  sc <- sequence $ flip fmap superclass $ \sc -> do
    sc' <- evaluate i sc
    case fromDynamic sc' of
      Just cs -> return cs
      Nothing -> throwIO (RuntimeError (variableName sc) "Superclass must be a class.")
  define (interpreterEnvironment i) (tokenLexeme name) (toDyn ())
  environment <- case sc of
    Just sc -> do
      environment <- childEnvironment (interpreterEnvironment i)
      define environment "super" (toDyn sc)
      return environment
    Nothing -> return (interpreterEnvironment i)
  methods <- fmap H.fromList $ forM methods $ \method -> do
    function <- newFunction method environment (tokenLexeme (functionName method) == "init")
    return (tokenLexeme (functionName method),function)
  klass <- newClass (tokenLexeme name) sc methods
  assign (interpreterEnvironment i) name (toDyn klass)
execute i (Expression expr) = void $ evaluate i expr
execute i f@(Function name _ _) =
  define (interpreterEnvironment i) (tokenLexeme name) .
    toDyn =<< newFunction f (interpreterEnvironment i) False
execute i (If condition thenBranch elseBranch) = do
  c <- isTruthy <$> evaluate i condition
  if c then execute i thenBranch
    else maybe (pure ()) (execute i) elseBranch
execute i (Print value) = do
  v <- evaluate i value
  putStrLn (stringify v)
execute i (Stmt.Return _ value) = do
  v <- maybe (return (toDyn ())) (evaluate i) value
  throwIO (Return.Return v)
execute i (Var name initializer) = do
  value <- maybe (return (toDyn ())) (evaluate i) initializer
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

lookupVariable :: Interpreter -> Token -> Expr -> IO Dynamic
lookupVariable i name expr = do
  distance <- H.lookup expr <$> readIORef (interpreterLocals i)
  case distance of
    Just d -> getAt (interpreterEnvironment i) d (tokenLexeme name)
    Nothing -> get globals name

checkNumberOperand :: Token -> Dynamic -> IO ()
checkNumberOperand operator operand
  | Just (_ :: Double) <- fromDynamic operand = return ()
  | otherwise = throwIO (RuntimeError operator "Operand must be a number.")

checkNumberOperands :: Token -> Dynamic -> Dynamic -> IO ()
checkNumberOperands operator left right
  | Just (_ :: Double) <- fromDynamic left
  , Just (_ :: Double) <- fromDynamic right = return ()
  | otherwise = throwIO (RuntimeError operator "Operands must be numbers.")

isTruthy v | Just () <- fromDynamic v = False
isTruthy v = fromDyn v True

isEqual :: Dynamic -> Dynamic -> Bool
isEqual a b
  | Just () <- fromDynamic a, Just () <- fromDynamic b = True
  | Just (da :: Double) <- fromDynamic a, Just db <- fromDynamic b = da == db
  | Just (ba :: Bool) <- fromDynamic a, Just bb <- fromDynamic b = ba == bb
  | Just (sa :: String) <- fromDynamic a, Just sb <- fromDynamic b = sa == sb
  | Just (IsCallable ca) <- dynToCallable a, Just (IsCallable cb) <- dynToCallable b = callableId ca == callableId cb
  | otherwise = False

stringify :: Dynamic -> String
stringify v
  | Just () <- fromDynamic v = "nil"
  | Just (b :: Bool) <- fromDynamic v = map toLower (show b)
  | Just (d :: Double) <- fromDynamic v = case showFFloat Nothing d "" of
      s | ".0" `isSuffixOf` s -> init (init s)
        | otherwise -> s
  | Just (s :: String) <- fromDynamic v = s
  | Just (IsCallable c) <- dynToCallable v = toString c
  | Just (i :: LoxInstance) <- fromDynamic v = LoxClass.className (instanceClass i) ++ " instance"

data Native = Native { nativeArity :: Int
                     , nativeFn :: [Dynamic] -> IO Dynamic
                     , nativeId :: Unique }
instance Callable Native where
  arity = nativeArity
  call n _ vs = (nativeFn n) vs
  toString _ = "<native fn>"
  callableId = nativeId

nativeClock :: [Dynamic] -> IO Dynamic
nativeClock [] = do
  TimeSpec sec nsec <- getTime Monotonic
  return $ toDyn (fromIntegral sec + fromIntegral nsec / 10^9 :: Double)

instance Hashable TT.TokenType
instance Hashable Literal
instance Hashable Token
instance Hashable Unique'
instance Hashable Expr

data IsCallable = forall c. Callable c => IsCallable c

dynToCallable :: Dynamic -> Maybe IsCallable
dynToCallable d = IsCallable <$> (fromDynamic d :: Maybe LoxFunction) <|>
                  IsCallable <$> (fromDynamic d :: Maybe Native) <|>
                  IsCallable <$> (fromDynamic d :: Maybe LoxClass)
