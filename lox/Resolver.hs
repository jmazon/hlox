module Resolver where

import qualified Data.HashMap.Strict as H
import Data.HashMap.Strict (HashMap)
import Data.IORef
import Data.List
import Data.Maybe
import Control.Monad

import Token
import Expr
import Stmt
import {-# SOURCE #-} Interpreter
import Misc

data FunctionType = FT_None | FT_Function | FT_Method | FT_Initializer
data ClassType = CT_None | CT_Class

data Resolver = Resolver
                { resolverInterpreter :: Interpreter
                , resolverScopes :: Stack (HashMap String Bool)
                , resolverCurrentFunction :: IORef FunctionType
                , resolverCurrentClass :: IORef ClassType }

newResolver :: Interpreter -> IO Resolver
newResolver i = liftM3 (Resolver i)
                  emptyStack (newIORef FT_None) (newIORef CT_None)

resolveS :: Resolver -> Stmt -> IO ()
resolveS r (Block statements) = do
  beginScope r
  mapM_ (resolveS r) statements
  endScope r
resolveS r (Class name methods) = do
  enclosingClass <- readIORef (resolverCurrentClass r)
  writeIORef (resolverCurrentClass r) CT_Class
  declare r name
  define r name
  beginScope r
  s <- pop (resolverScopes r)
  push (resolverScopes r) (H.insert "this" True s)
  forM_ methods $ \method -> do
    let declaration | tokenLexeme (functionName method) == "init" = FT_Initializer
                    | otherwise = FT_Method
    resolveFunction r method declaration
  endScope r
  writeIORef (resolverCurrentClass r) enclosingClass
resolveS r (Expression expression) = resolveE r expression
resolveS r stmt@(Function name _ _) = do
  declare r name
  define r name
  resolveFunction r stmt FT_Function
resolveS r (If condition thenBranch elseBranch) = do
  resolveE r condition
  resolveS r thenBranch
  maybe (return ()) (resolveS r) elseBranch
resolveS r (Print expression) = resolveE r expression
resolveS r (Return keyword value) = do
  cf <- readIORef (resolverCurrentFunction r)
  case cf of
    FT_None -> void $ tokenError keyword "Cannot return from top-level code."
    _ -> return ()
  when (isJust value) $ do
    case cf of FT_Initializer -> void $ tokenError keyword "Cannot return a value from an initializer."
               _ -> return ()
    resolveE r (fromJust value)
resolveS r (Var name initializer) = do
  declare r name
  maybe (return ()) (resolveE r) initializer
  define r name
resolveS r (While condition body) = do
  resolveE r condition
  resolveS r body

resolveE :: Resolver -> Expr -> IO ()
resolveE r expr@(Assign name _ value) = do
  resolveE r value
  resolveLocal r expr name
resolveE r (Binary left _ right) = do
  resolveE r left
  resolveE r right
resolveE r (Call callee _ arguments) = do
  resolveE r callee
  mapM_ (resolveE r) arguments
resolveE r (Get object _) = resolveE r object
resolveE r (Grouping expression) = resolveE r expression
resolveE _ (Literal _) = return ()
resolveE r (Logical left _ right) = do
  resolveE r left
  resolveE r right
resolveE r (Set object _ value) = do
  resolveE r value
  resolveE r object
resolveE r expr@(This keyword _) = do
  cc <- readIORef (resolverCurrentClass r)
  case cc of CT_None -> void $ tokenError keyword "Cannot use 'this' outside of a class."
             _ -> resolveLocal r expr keyword
resolveE r (Unary _ right) = resolveE r right
resolveE r expr@(Variable name _) = do
  whenM (not <$> isEmpty (resolverScopes r)) $
    whenM ((== Just False) . H.lookup (tokenLexeme name) <$> peek (resolverScopes r)) $ do
    void $ tokenError name "Cannot read local variable in its own initializer."
  resolveLocal r expr name

resolveFunction :: Resolver -> Stmt -> FunctionType -> IO ()
resolveFunction r function ftype = do
  enclosingFunction <- readIORef (resolverCurrentFunction r)
  writeIORef (resolverCurrentFunction r) ftype
  beginScope r
  forM_ (functionParams function) $ \param -> do
    declare r param
    define r param
  mapM_ (resolveS r) (functionBody function)
  endScope r
  writeIORef (resolverCurrentFunction r) enclosingFunction

beginScope :: Resolver -> IO ()
beginScope r = push (resolverScopes r) H.empty

endScope :: Resolver -> IO ()
endScope r = void $ pop (resolverScopes r)

declare :: Resolver -> Token -> IO ()
declare r name = unlessM (isEmpty (resolverScopes r)) $ do
  scope <- peek (resolverScopes r)
  if tokenLexeme name `H.member` scope then
    void $ tokenError name "Variable with this name already declared in this scope."
    else do
    pop (resolverScopes r)
    push (resolverScopes r) (H.insert (tokenLexeme name) False scope)

define :: Resolver -> Token -> IO ()
define r name = unlessM (isEmpty (resolverScopes r)) $ do
  scope <- pop (resolverScopes r)
  push (resolverScopes r) (H.insert (tokenLexeme name) True scope)

resolveLocal :: Resolver -> Expr -> Token -> IO ()
resolveLocal r expr name = do
  f <- findIndex (tokenLexeme name `H.member`) <$> frames (resolverScopes r)
  maybe (return ()) (resolveI (resolverInterpreter r) expr) f

newtype Stack a = Stack (IORef [a])

emptyStack :: IO (Stack a)
emptyStack = Stack <$> newIORef []

push :: Stack a -> a -> IO ()
push (Stack r) value = modifyIORef r (value :)

peek :: Stack a -> IO a
peek (Stack r) = head <$> readIORef r

pop :: Stack a -> IO a
pop (Stack r) = do
  top <- head <$> readIORef r
  modifyIORef r tail
  return top

isEmpty :: Stack a -> IO Bool
isEmpty (Stack r) = null <$> readIORef r

frames :: Stack a -> IO [a]
frames (Stack r) = readIORef r

whenM :: Monad m => m Bool -> m () -> m ()
whenM c b = do p <- c
               when p b

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM c b = do p <- c
                 unless p b
