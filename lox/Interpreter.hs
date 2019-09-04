{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Interpreter (Interpreter(Interpreter),newInterpreter,interpret,executeBlock,resolveLocals,MI) where

import Data.List (foldl')
import qualified Data.HashMap.Strict as H
import Data.HashMap.Strict (HashMap)
import Control.Monad
import Control.Exception hiding (evaluate)
import System.Clock (TimeSpec(TimeSpec),getTime,Clock(Monotonic))
import Numeric
import Data.Unique
import Data.Dynamic
import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad.Reader
import Control.Monad.Except

import Util
import Token (Token,tokenLexeme)
import Expr
import RuntimeError (RuntimeError(RuntimeError))
import Stmt
import Environment (Environment,newEnvironment,define,getAt,assignAt,assign,childEnvironment,get)
import LoxClass (LoxClass,newClass,className,findMethod)
import LoxInstance (LoxInstance,getP,setP,instanceClass)
import LoxFunction (LoxFunction,newFunction,bind)
import Return (Return(Return))
import LoxCallable (LoxCallable,arity,call,toString,callableId)

type MI = ExceptT Return (ReaderT Interpreter IO)

data Interpreter = Interpreter { globals :: Environment
                               , environment :: Environment
                               , locals :: HashMap ExprKey Int}

newInterpreter :: IO Interpreter
newInterpreter = do
  g <- do
    g <- newEnvironment
    clock <- toDyn . Native 0 nativeClock <$> newUnique
    define "clock" clock g
    return g
  return $ Interpreter g g H.empty

interpret :: Interpreter -> [Stmt] -> IO (Maybe RuntimeError)
interpret i statements = handle (return . Just) $ do
  r <- runReaderT (runExceptT (mapM_ execute statements)) i
  case r of Right () -> return Nothing
            Left (Return.Return _) -> error "Internal error: leaked return."

evaluate :: Expr -> MI Dynamic
evaluate (Literal l) = return l
evaluate (Logical left operator right) = do
  l <- evaluate left
  case (operator,isTruthy l) of
    (LogOr,True) -> return l
    (LogAnd,False) -> return l
    _ -> evaluate right
evaluate (Get object name) = do
  o <- evaluate object
  case fromDynamic o of Just inst -> getP inst name
                        Nothing -> throwIO' (RuntimeError name "Only instances have properties.")
evaluate (Set object name value) = do
  o <- evaluate object
  case fromDynamic o of Just inst -> do v <- evaluate value
                                        setP inst name v
                                        return v
                        Nothing -> throwIO' (RuntimeError name "Only instances have fields.")
evaluate (Super _ key method) = do
  Just distance <- H.lookup key <$> asks locals
  Just superclass <- fmap fromDynamic . getAt distance "super" =<< asks environment
  -- "this" is always one level nearer than "super"'s environment
  Just object <- fmap fromDynamic . getAt (distance - 1) "this" =<< asks environment
  case findMethod superclass (tokenLexeme method) of
    Just m -> toDyn <$> bind object m
    Nothing -> throwIO' (RuntimeError method (T.concat ["Undefined property '",tokenLexeme method,"'."]))
evaluate (This keyword key) = lookupVariable keyword key
evaluate (Grouping expr) = evaluate expr
evaluate (Unary operator token right) = do
  r <- evaluate right
  case operator of
    UnaryBang -> return (toDyn (not (isTruthy r)))
    UnaryMinus -> toDyn . negate <$> getNumberOperand token r
evaluate (Variable name key) = lookupVariable name key
evaluate (Assign name key value) = do
  v <- evaluate value
  distance <- H.lookup key <$> asks locals
  case distance of Just d -> assignAt d name v =<< asks environment
                   Nothing -> assign name v =<< asks globals
  return v
evaluate (Binary left token operator right) = do
  l <- evaluate left
  r <- evaluate right
  case operator of
    BinGreater      -> toDyn . uncurry (>)  <$> getNumberOperands token l r
    BinGreaterEqual -> toDyn . uncurry (>=) <$> getNumberOperands token l r
    BinLess         -> toDyn . uncurry (<)  <$> getNumberOperands token l r
    BinLessEqual    -> toDyn . uncurry (<=) <$> getNumberOperands token l r

    BinBangEqual    -> return (toDyn (not (isEqual l r)))
    BinEqualEqual   -> return (toDyn (isEqual l r))

    BinPlus | Just vl <- fromDynamic l, Just vr <- fromDynamic r -> return (toDyn (vl + vr :: Double))
            | Just vl <- fromDynamic l, Just vr <- fromDynamic r -> return (toDyn (vl `T.append` vr))
            | otherwise -> throwIO' (RuntimeError token "Operands must be two numbers or two strings.")
    BinMinus -> toDyn . uncurry (-) <$> getNumberOperands token l r
    BinSlash -> toDyn . uncurry (/) <$> getNumberOperands token l r
    BinStar  -> toDyn . uncurry (*) <$> getNumberOperands token l r
evaluate (Call callee paren arguments) = do
  c <- evaluate callee
  as <- mapM evaluate arguments
  case dynToCallable c of
    Nothing -> throwIO' $ RuntimeError paren "Can only call functions and classes."
    Just (IsCallable function) -> do
      when (length arguments /= arity function) $
        throwIO' $ RuntimeError paren $ T.concat [ "Expected "
                                                , T.pack (show (arity function))
                                                , " arguments but got "
                                                , T.pack (show (length arguments))
                                                , "." ]
      call function as

execute :: Stmt -> MI ()
execute (Class name superclass methods) = do
  sc <- sequence $ flip fmap superclass $ \scVar -> do
    scVal <- evaluate scVar
    case fromDynamic scVal of
      Just scClass -> return scClass
      Nothing -> throwIO' (RuntimeError (variableName scVar) "Superclass must be a class.")
  define (tokenLexeme name) (toDyn ()) =<< asks environment
  env <- case sc of
    Just sc' -> do
      env <- childEnvironment =<< asks environment
      define "super" (toDyn sc') env
      return env
    Nothing -> asks environment
  methods' <- fmap H.fromList $ forM methods $ \method -> do
    function <- newFunction method env (tokenLexeme (functionName method) == "init")
    return (tokenLexeme (functionName method),function)
  klass <- newClass (tokenLexeme name) sc methods'
  assign name (toDyn klass) =<< asks environment
execute (Expression expr) = void $ evaluate expr
execute (Function f@(FunDecl name _ _)) = do
  e <- asks environment
  fun <- toDyn <$> newFunction f e False
  define (tokenLexeme name) fun e
execute (If condition thenBranch elseBranch) = do
  c <- isTruthy <$> evaluate condition
  if c then execute thenBranch
       else mapM_ execute elseBranch
execute (Print value) = do
  v <- evaluate value
  liftIO $ T.putStrLn (stringify v)
execute (Stmt.Return _ value) = do
  v <- maybe (return (toDyn ())) evaluate value
  throwError (Return.Return v)
execute (Var name initializer) = do
  value <- maybe (return (toDyn ())) evaluate initializer
  define (tokenLexeme name) value =<< asks environment
execute (While condition body) =
  whileM_ (isTruthy <$> evaluate condition) $ execute body
execute (Block statements) = executeBlock statements =<<
                             childEnvironment =<< asks environment

executeBlock :: [Stmt] -> Environment -> MI ()
executeBlock statements env =
  local (\i -> i { environment = env }) $ mapM_ execute statements

resolveLocals :: Foldable f => Interpreter -> f (ExprKey,Int) -> Interpreter
resolveLocals i kvs =
  i { locals = foldl' (\h (k,v) -> H.insert k v h) (locals i) kvs }

lookupVariable :: Token -> ExprKey -> MI Dynamic
lookupVariable name key = do
  distance <- H.lookup key <$> asks locals
  case distance of
    Just d -> getAt d (tokenLexeme name) =<< asks environment
    Nothing -> get name =<< asks globals

getNumberOperand :: MonadIO m => Token -> Dynamic -> m Double
getNumberOperand operator operand
  | Just (d :: Double) <- fromDynamic operand = return d
  | otherwise = throwIO' (RuntimeError operator "Operand must be a number.")

getNumberOperands :: MonadIO m => Token -> Dynamic -> Dynamic -> m (Double,Double)
getNumberOperands operator left right
  | Just (a :: Double) <- fromDynamic left
  , Just (b :: Double) <- fromDynamic right = return (a,b)
  | otherwise = throwIO' (RuntimeError operator "Operands must be numbers.")

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
                     , nativeFn :: [Dynamic] -> MI Dynamic
                     , nativeId :: Unique }
instance LoxCallable Native where
  arity = nativeArity
  call = nativeFn
  toString _ = "<native fn>"
  callableId = nativeId

nativeClock :: [Dynamic] -> MI Dynamic
nativeClock [] = do
  TimeSpec sec nsec <- liftIO (getTime Monotonic)
  return $ toDyn (fromIntegral sec + fromIntegral nsec / 1000000000 :: Double)
nativeClock _ = error "Internal error: clock called with arguments" -- XXX

data IsCallable = forall c. LoxCallable c => IsCallable c

dynToCallable :: Dynamic -> Maybe IsCallable
dynToCallable d = IsCallable <$> (fromDynamic d :: Maybe LoxFunction) <|>
                  IsCallable <$> (fromDynamic d :: Maybe Native) <|>
                  IsCallable <$> (fromDynamic d :: Maybe LoxClass)

throwIO' :: MonadIO m => RuntimeError -> m a
throwIO' = liftIO . throwIO
