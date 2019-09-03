{-# LANGUAGE OverloadedStrings #-}
module Resolver (resolve) where

import qualified Data.HashMap.Strict as H
import Data.HashMap.Strict (HashMap)
import Data.List
import Control.Monad
import Data.Text (Text)
import Control.Monad.RWS.Strict
import Data.DList

import Util
import Token (Token,tokenLexeme)
import Expr
import Stmt

data FunctionType = FT_None | FT_Function | FT_Method | FT_Initializer
data ClassType = CT_None | CT_Class | CT_Subclass
{-# ANN type FunctionType ("HLint: ignore Use camelCase" :: String) #-}
{-# ANN type ClassType ("HLint: ignore Use camelCase" :: String) #-}

data ResolverCurrents = ResolverCurrents { currentFunction :: FunctionType
                                         , currentClass :: ClassType }
type MR = RWS ResolverCurrents (DList (ExprKey,Int),DList (Token,Text))
                               (Stack (HashMap Text Bool))

resolve :: [Stmt] -> (DList (ExprKey,Int),DList (Token,Text))
resolve statements = snd $ execRWS (mapM_ resolveS statements)
                             (ResolverCurrents FT_None CT_None) emptyStack

resolverError :: Token -> Text -> MR ()
resolverError tok msg = tell (empty,singleton (tok,msg))

resolveS :: Stmt -> MR ()
resolveS (Block statements) = withScope Nothing $ mapM_ resolveS statements
resolveS (Class name superclass methods) = do
  -- as opposed to jlox, those two performed before currentClass is CT_Class:
  declare name
  define name
  scope <- case superclass of
    Nothing -> return $ withCurrentClass CT_Class
    Just sc -> do
      when (tokenLexeme name == tokenLexeme (variableName sc)) $
        resolverError (variableName sc) "A class cannot inherit from itself."
      -- as opposed to jlox, this performed before currentClass is CT_Subclass:
      resolveE sc
      return $ withCurrentClass CT_Subclass . withScope (Just ("super",True))
  scope $ withScope (Just ("this",True)) $
    forM_ methods $ \method -> do
      let declaration | tokenLexeme (functionName method) == "init" = FT_Initializer
                      | otherwise = FT_Method
      resolveFunction method declaration
resolveS (Expression expression) = resolveE expression
resolveS (Function f@(FunDecl name _ _)) = do
  declare name
  define name
  resolveFunction f FT_Function
resolveS (If condition thenBranch elseBranch) = do
  resolveE condition
  resolveS thenBranch
  mapM_ resolveS elseBranch
resolveS (Print expression) = resolveE expression
resolveS (Return keyword value) = do
  cf <- asks currentFunction
  case cf of
    FT_None -> resolverError keyword "Cannot return from top-level code."
    FT_Initializer -> sequence_ $ resolverError keyword "Cannot return a value from an initializer." <$ value
    _ -> return ()
  mapM_ resolveE value
resolveS (Var name initializer) = do
  declare name
  mapM_ resolveE initializer
  define name
resolveS (While condition body) = do
  resolveE condition
  resolveS body

resolveE :: Expr -> MR ()
resolveE (Assign name key value) = do
  resolveE value
  resolveLocal key name
resolveE (Binary left _ _ right) = do
  resolveE left
  resolveE right
resolveE (Call callee _ arguments) = do
  resolveE callee
  mapM_ resolveE arguments
resolveE (Get object _) = resolveE object
resolveE (Grouping expression) = resolveE expression
resolveE (Literal _) = return ()
resolveE (Logical left _ right) = do
  resolveE left
  resolveE right
resolveE (Set object _ value) = do
  resolveE value
  resolveE object
resolveE (Super keyword key _) = do
  cc <- asks currentClass
  case cc of
    CT_None -> resolverError keyword "Cannot use 'super' outside of a class."
    -- simplified from jlox
    CT_Class -> resolverError keyword "Cannot use 'super' in a class with no superclass."
    CT_Subclass -> return ()
  resolveLocal key keyword
resolveE (This keyword key) = do
  cc <- asks currentClass
  case cc of CT_None -> resolverError keyword "Cannot use 'this' outside of a class."
             _ -> resolveLocal key keyword
resolveE (Unary _ _ right) = resolveE right
resolveE (Variable name key) = do
  whenM (not . isEmpty <$> get) $
    whenM ((== Just False) . H.lookup (tokenLexeme name) . peek <$> get) $
      resolverError name "Cannot read local variable in its own initializer."
  resolveLocal key name

resolveFunction :: FunDecl -> FunctionType -> MR ()
resolveFunction function ftype =
  withCurrentFunction ftype $ withScope Nothing $ do
    forM_ (functionParams function) $ \param -> do
      declare param
      define param
    mapM_ resolveS (functionBody function)

withScope :: Maybe (Text,Bool) -> MR () -> MR ()
withScope top body = do
  modify $ push $ maybe H.empty (uncurry H.singleton) top
  body
  modify pop

withCurrentFunction :: FunctionType -> MR () -> MR ()
withCurrentFunction ftype = local (\cs -> cs { currentFunction = ftype })

withCurrentClass :: ClassType -> MR () -> MR ()
withCurrentClass ctype = local (\cs -> cs { currentClass = ctype })

declare :: Token -> MR ()
declare name = unlessM (isEmpty <$> get) $ do
  scope <- peek <$> get
  if tokenLexeme name `H.member` scope then
    resolverError name "Variable with this name already declared in this scope."
  else modify . modifyTop $ H.insert (tokenLexeme name) False

define :: Token -> MR ()
define name = unlessM (isEmpty <$> get) $
  modify . modifyTop $ H.insert (tokenLexeme name) True

resolveLocal :: ExprKey -> Token -> MR ()
resolveLocal key name = do
  f <- findIndex (tokenLexeme name `H.member`) . frames <$> get
  mapM_ (\f' -> tell (singleton (key,f'),empty)) f

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
