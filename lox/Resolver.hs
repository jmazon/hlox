{-# LANGUAGE OverloadedStrings #-}
module Resolver (resolve) where

import qualified Data.HashMap.Strict as H
import Data.HashMap.Strict (HashMap)
import Data.IORef
import Data.List
import Data.Maybe
import Control.Monad
import Data.Text (Text)
import Control.Monad.Writer
import Data.DList

import Util
import Token (Token,tokenLexeme)
import Expr
import Stmt

data FunctionType = FT_None | FT_Function | FT_Method | FT_Initializer
data ClassType = CT_None | CT_Class | CT_Subclass
{-# ANN type FunctionType ("HLint: ignore Use camelCase" :: String) #-}
{-# ANN type ClassType ("HLint: ignore Use camelCase" :: String) #-}

data Resolver = Resolver
                { resolverLocals :: IORef [(ExprKey,Int)]
                , resolverScopes :: IORef (Stack (HashMap Text Bool))
                , resolverCurrentFunction :: IORef FunctionType
                , resolverCurrentClass :: IORef ClassType }

type ResolverError = (Token,Text)
type MR = WriterT (DList ResolverError) IO

resolve :: [Stmt] -> IO ([(ExprKey,Int)],DList ResolverError)
resolve statements = runWriterT $ do
  r <- liftIO newResolver
  mapM_ (resolveS r) statements
  liftIO $ readIORef (resolverLocals r)

newResolver :: IO Resolver
newResolver = liftM4 Resolver (newIORef []) (newIORef emptyStack)
                       (newIORef FT_None) (newIORef CT_None)

resolverError :: Resolver -> Token -> Text -> MR ()
resolverError _ tok msg = tell $ singleton (tok,msg)

resolveS :: Resolver -> Stmt -> MR ()
resolveS r (Block statements) = withScope r Nothing $ mapM_ (resolveS r) statements
resolveS r (Class name superclass methods) = do
  -- as opposed to jlox, those two performed before currentClass is CT_Class:
  declare r name
  define r name
  scope <- case superclass of
    Nothing -> return $ withCurrentClass r CT_Class
    Just sc -> do
      when (tokenLexeme name == tokenLexeme (variableName sc)) $
        resolverError r (variableName sc) "A class cannot inherit from itself."
      -- as opposed to jlox, this performed before currentClass is CT_Subclass:
      resolveE r sc
      return $ withCurrentClass r CT_Subclass . withScope r (Just ("super",True))
  scope $ withScope r (Just ("this",True)) $
    forM_ methods $ \method -> do
      let declaration | tokenLexeme (functionName method) == "init" = FT_Initializer
                      | otherwise = FT_Method
      resolveFunction r method declaration
resolveS r (Expression expression) = resolveE r expression
resolveS r (Function f@(FunDecl name _ _)) = do
  declare r name
  define r name
  resolveFunction r f FT_Function
resolveS r (If condition thenBranch elseBranch) = do
  resolveE r condition
  resolveS r thenBranch
  maybe (return ()) (resolveS r) elseBranch
resolveS r (Print expression) = resolveE r expression
resolveS r (Return keyword value) = do
  cf <- liftIO $ readIORef (resolverCurrentFunction r)
  case cf of
    FT_None -> resolverError r keyword "Cannot return from top-level code."
    _ -> return ()
  when (isJust value) $ do
    case cf of FT_Initializer -> resolverError r keyword "Cannot return a value from an initializer."
               _ -> return ()
    resolveE r (fromJust value)
resolveS r (Var name initializer) = do
  declare r name
  maybe (return ()) (resolveE r) initializer
  define r name
resolveS r (While condition body) = do
  resolveE r condition
  resolveS r body

resolveE :: Resolver -> Expr -> MR ()
resolveE r (Assign name key value) = do
  resolveE r value
  resolveLocal r key name
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
resolveE r (Super keyword key _) = do
  cc <- liftIO $ readIORef (resolverCurrentClass r)
  case cc of
    CT_None -> resolverError r keyword "Cannot use 'super' outside of a class."
    _ -> case cc of
      CT_Subclass -> return ()
      _ -> resolverError r keyword "Cannot use 'super' in a class with no superclass."
  resolveLocal r key keyword
resolveE r (This keyword key) = do
  cc <- liftIO $ readIORef (resolverCurrentClass r)
  case cc of CT_None -> resolverError r keyword "Cannot use 'this' outside of a class."
             _ -> resolveLocal r key keyword
resolveE r (Unary _ right) = resolveE r right
resolveE r (Variable name key) = do
  whenM (liftIO $ not . isEmpty <$> readIORef (resolverScopes r)) $
    whenM (liftIO $ (== Just False) . H.lookup (tokenLexeme name) . peek <$>
           readIORef (resolverScopes r)) $
      resolverError r name "Cannot read local variable in its own initializer."
  resolveLocal r key name

resolveFunction :: Resolver -> FunDecl -> FunctionType -> MR ()
resolveFunction r function ftype =
  withCurrentFunction r ftype $ withScope r Nothing $ do
    forM_ (functionParams function) $ \param -> do
      declare r param
      define r param
    mapM_ (resolveS r) (functionBody function)

withScope :: Resolver -> Maybe (Text,Bool) -> MR () -> MR ()
withScope r top body = do
  beginScope r top
  body
  endScope r

beginScope :: Resolver -> Maybe (Text,Bool) -> MR ()
beginScope r top = liftIO $ modifyIORef (resolverScopes r) $ push $
                   maybe H.empty (uncurry H.singleton) top

endScope :: Resolver -> MR ()
endScope r = liftIO $ modifyIORef (resolverScopes r) pop

withCurrentFunction :: Resolver -> FunctionType -> MR () -> MR ()
withCurrentFunction r ftype body = do
  enclosingFunction <- liftIO $ readIORef (resolverCurrentFunction r)
  liftIO $ writeIORef (resolverCurrentFunction r) ftype
  body
  liftIO $ writeIORef (resolverCurrentFunction r) enclosingFunction

withCurrentClass :: Resolver -> ClassType -> MR () -> MR ()
withCurrentClass r ctype body = do
  enclosingClass <- liftIO $ readIORef (resolverCurrentClass r)
  liftIO $ writeIORef (resolverCurrentClass r) ctype
  body
  liftIO $ writeIORef (resolverCurrentClass r) enclosingClass

declare :: Resolver -> Token -> MR ()
declare r name = unlessM (liftIO $ isEmpty <$> readIORef (resolverScopes r)) $ do
  scope <- liftIO $ peek <$> readIORef (resolverScopes r)
  if tokenLexeme name `H.member` scope then
    resolverError r name "Variable with this name already declared in this scope."
  else liftIO $ modifyIORef (resolverScopes r) . modifyTop $ H.insert (tokenLexeme name) False

define :: Resolver -> Token -> MR ()
define r name = liftIO $ unlessM (isEmpty <$> readIORef (resolverScopes r)) $
  modifyIORef (resolverScopes r) . modifyTop $ H.insert (tokenLexeme name) True

resolveLocal :: Resolver -> ExprKey -> Token -> MR ()
resolveLocal r key name = do
  f <- liftIO $ findIndex (tokenLexeme name `H.member`) . frames <$>
                readIORef (resolverScopes r)
  mapM_ (\f' -> liftIO $ modifyIORef (resolverLocals r) ((key,f'):)) f

newtype Stack a = Stack [a]

emptyStack :: Stack a
emptyStack = Stack []

push :: a -> Stack a -> Stack a
push value (Stack r) = Stack (value : r)

peek :: Stack a -> a
peek (Stack (h:_)) = h
peek _ = error "Internal error: peek on empty Stack"

pop :: Stack a -> Stack a
pop (Stack (_:t)) = Stack t
pop _ = error "Internal error: pop on empty Stack"

isEmpty :: Stack a -> Bool
isEmpty (Stack r) = null r

frames :: Stack a -> [a]
frames (Stack r) = r

modifyTop :: (a -> a) -> Stack a -> Stack a
modifyTop f (Stack (h:t)) = Stack (f h : t)
modifyTop _ _ = error "Internal error: modifyTop on empty Stack"
