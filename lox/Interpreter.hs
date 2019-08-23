{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Interpreter (Interpreter(Interpreter),newInterpreter,interpret,executeBlock,resolveLocals) where

import Data.List (foldl')
import qualified Data.HashMap.Strict as H
import Data.HashMap.Strict (HashMap)
import Data.IORef
import Control.Monad
import Control.Exception hiding (evaluate)
import System.IO.Unsafe
import System.Clock (TimeSpec(TimeSpec),getTime,Clock(Monotonic))
import Numeric
import Data.Unique
import Data.Dynamic
import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Util
import qualified TokenType as TT
import Token (Token,tokenType,tokenLexeme,Literal(LNull,LBool,LNumber,LString))
import Expr
import RuntimeError (RuntimeError(RuntimeError))
import Stmt
import Environment (Environment,newEnvironment,define,getAt,assignAt,assign,childEnvironment,get)
import LoxClass (LoxClass,newClass,className,findMethod)
import LoxInstance (LoxInstance,getP,setP,instanceClass)
import LoxFunction (LoxFunction,newFunction,bind)
import Return (Return(Return))
import LoxCallable (LoxCallable,arity,call,toString,callableId)

data Interpreter = Interpreter { interpreterEnvironment :: Environment
                               , interpreterLocals :: IORef (HashMap Unique Int)}

globals :: Environment
{-# NOINLINE globals #-}
globals = unsafePerformIO $ do
  g <- newEnvironment
  define g "clock" . toDyn . Native 0 nativeClock =<< newUnique
  return g

newInterpreter :: IO Interpreter
newInterpreter = Interpreter globals <$> newIORef H.empty

interpret :: (RuntimeError -> IO ()) -> Interpreter -> [Stmt] -> IO ()
interpret runtimeError i statements =
  mapM_ (execute i) statements `catch` runtimeError

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
evaluate i (Super _ key method) = do
  Just distance <- H.lookup key <$> readIORef (interpreterLocals i)
  Just superclass <- fromDynamic <$> getAt (interpreterEnvironment i) distance "super"
  -- "this" is always one level nearer than "super"'s environment
  Just object <- fromDynamic <$> getAt (interpreterEnvironment i) (distance - 1) "this"
  case findMethod superclass (tokenLexeme method) of
    Just m -> toDyn <$> bind m object
    Nothing -> throwIO (RuntimeError method (T.concat ["Undefined property '",tokenLexeme method,"'."]))
evaluate i (This keyword key) = lookupVariable i keyword key
evaluate i (Grouping expr) = evaluate i expr
evaluate i (Unary operator right) = do
  r <- evaluate i right
  case tokenType operator of
    TT.Bang -> return (toDyn (not (isTruthy r)))
    TT.Minus -> toDyn . negate <$> getNumberOperand operator r
    _ -> return (toDyn ()) -- XXX why?
evaluate i (Variable name key) = lookupVariable i name key
evaluate i (Assign name key value) = do
  v <- evaluate i value
  distance <- H.lookup key <$> readIORef (interpreterLocals i)
  case distance of Just d -> assignAt (interpreterEnvironment i) d name v
                   Nothing -> assign globals name v
  return v
evaluate i (Binary left operator right) = do
  l <- evaluate i left
  r <- evaluate i right
  case tokenType operator of
    TT.Greater -> toDyn . uncurry (>) <$> getNumberOperands operator l r
    TT.GreaterEqual -> toDyn . uncurry (>=) <$> getNumberOperands operator l r
    TT.Less -> toDyn .uncurry (<) <$> getNumberOperands operator l r
    TT.LessEqual -> toDyn . uncurry (<=) <$> getNumberOperands operator l r

    TT.BangEqual -> return (toDyn (not (isEqual l r)))
    TT.EqualEqual -> return (toDyn (isEqual l r))

    TT.Plus | Just vl <- fromDynamic l, Just vr <- fromDynamic r -> return (toDyn (vl + vr :: Double))
            | Just vl <- fromDynamic l, Just vr <- fromDynamic r -> return (toDyn (vl `T.append` vr))
            | otherwise -> throwIO (RuntimeError operator "Operands must be two numbers or two strings.")
    TT.Minus -> toDyn . uncurry (-) <$> getNumberOperands operator l r
    TT.Slash -> toDyn . uncurry (/) <$> getNumberOperands operator l r
    TT.Star -> toDyn . uncurry (*) <$> getNumberOperands operator l r
    _ -> return (toDyn ()) -- XXX why?
evaluate i (Call callee paren arguments) = do
  c <- evaluate i callee
  as <- mapM (evaluate i) arguments
  case dynToCallable c of
    Nothing -> throwIO $ RuntimeError paren "Can only call functions and classes."
    Just (IsCallable function) -> do
      when (length arguments /= arity function) $
        throwIO $ RuntimeError paren $ T.concat [ "Expected "
                                                , T.pack (show (arity function))
                                                , " arguments but got "
                                                , T.pack (show (length arguments))
                                                , "." ]
      call function i as

execute :: Interpreter -> Stmt -> IO ()
execute i (Class name superclass methods) = do
  sc <- sequence $ flip fmap superclass $ \scVar -> do
    scVal <- evaluate i scVar
    case fromDynamic scVal of
      Just scClass -> return scClass
      Nothing -> throwIO (RuntimeError (variableName scVar) "Superclass must be a class.")
  define (interpreterEnvironment i) (tokenLexeme name) (toDyn ())
  environment <- case sc of
    Just sc' -> do
      environment <- childEnvironment (interpreterEnvironment i)
      define environment "super" (toDyn sc')
      return environment
    Nothing -> return (interpreterEnvironment i)
  methods' <- fmap H.fromList $ forM methods $ \method -> do
    function <- newFunction method environment (tokenLexeme (functionName method) == "init")
    return (tokenLexeme (functionName method),function)
  klass <- newClass (tokenLexeme name) sc methods'
  assign (interpreterEnvironment i) name (toDyn klass)
execute i (Expression expr) = void $ evaluate i expr
execute i (Function f@(FunDecl name _ _)) =
  define (interpreterEnvironment i) (tokenLexeme name) .
    toDyn =<< newFunction f (interpreterEnvironment i) False
execute i (If condition thenBranch elseBranch) = do
  c <- isTruthy <$> evaluate i condition
  if c then execute i thenBranch
    else maybe (pure ()) (execute i) elseBranch
execute i (Print value) = do
  v <- evaluate i value
  T.putStrLn (stringify v)
execute i (Stmt.Return _ value) = do
  v <- maybe (return (toDyn ())) (evaluate i) value
  throwIO (Return.Return v)
execute i (Var name initializer) = do
  value <- maybe (return (toDyn ())) (evaluate i) initializer
  define (interpreterEnvironment i) (tokenLexeme name) value
execute i (While condition body) =
  whileM_ (isTruthy <$> evaluate i condition) $ execute i body
execute i (Block statements) = executeBlock i statements =<<
                                 childEnvironment (interpreterEnvironment i)

executeBlock :: Interpreter -> [Stmt] -> Environment -> IO ()
executeBlock i statements environment =
  forM_ statements $ execute i { interpreterEnvironment = environment }

resolveLocals :: Interpreter -> [(Unique,Int)] -> IO ()
resolveLocals i kvs = modifyIORef (interpreterLocals i) $
                      \h0 -> foldl' (\h (k,v) -> H.insert k v h) h0 kvs

lookupVariable :: Interpreter -> Token -> Unique -> IO Dynamic
lookupVariable i name key = do
  distance <- H.lookup key <$> readIORef (interpreterLocals i)
  case distance of
    Just d -> getAt (interpreterEnvironment i) d (tokenLexeme name)
    Nothing -> get globals name

getNumberOperand :: Token -> Dynamic -> IO Double
getNumberOperand operator operand
  | Just (d :: Double) <- fromDynamic operand = return d
  | otherwise = throwIO (RuntimeError operator "Operand must be a number.")

getNumberOperands :: Token -> Dynamic -> Dynamic -> IO (Double,Double)
getNumberOperands operator left right
  | Just (a :: Double) <- fromDynamic left
  , Just (b :: Double) <- fromDynamic right = return (a,b)
  | otherwise = throwIO (RuntimeError operator "Operands must be numbers.")

isTruthy :: Dynamic -> Bool
isTruthy v | Just () <- fromDynamic v = False
isTruthy v = fromDyn v True

isEqual :: Dynamic -> Dynamic -> Bool
isEqual a b
  | Just () <- fromDynamic a, Just () <- fromDynamic b = True
  | Just (da :: Double) <- fromDynamic a, Just db <- fromDynamic b = da == db
  | Just (ba :: Bool) <- fromDynamic a, Just bb <- fromDynamic b = ba == bb
  | Just (ta :: Text) <- fromDynamic a, Just tb <- fromDynamic b = ta == tb
  | Just (IsCallable ca) <- dynToCallable a, Just (IsCallable cb) <- dynToCallable b = callableId ca == callableId cb
  | otherwise = False

stringify :: Dynamic -> Text
stringify v
  | Just () <- fromDynamic v = "nil"
  | Just (b :: Bool) <- fromDynamic v = T.toLower (T.pack (show b))
  | Just (d :: Double) <- fromDynamic v = case T.pack (showFFloat Nothing d "") of
      s | ".0" `T.isSuffixOf` s -> T.init (T.init s)
        | otherwise -> s
  | Just (t :: Text) <- fromDynamic v = t
  | Just (IsCallable c) <- dynToCallable v = toString c
  | Just (i :: LoxInstance) <- fromDynamic v = LoxClass.className (instanceClass i) `T.append` " instance"
  | otherwise = error "Internal error: unknown type to stringify" -- XXX

data Native = Native { nativeArity :: Int
                     , nativeFn :: [Dynamic] -> IO Dynamic
                     , nativeId :: Unique }
instance LoxCallable Native where
  arity = nativeArity
  call n _ = nativeFn n
  toString _ = "<native fn>"
  callableId = nativeId

nativeClock :: [Dynamic] -> IO Dynamic
nativeClock [] = do
  TimeSpec sec nsec <- getTime Monotonic
  return $ toDyn (fromIntegral sec + fromIntegral nsec / 1000000000 :: Double)
nativeClock _ = error "Internal error: clock called with arguments" -- XXX

data IsCallable = forall c. LoxCallable c => IsCallable c

dynToCallable :: Dynamic -> Maybe IsCallable
dynToCallable d = IsCallable <$> (fromDynamic d :: Maybe LoxFunction) <|>
                  IsCallable <$> (fromDynamic d :: Maybe Native) <|>
                  IsCallable <$> (fromDynamic d :: Maybe LoxClass)
