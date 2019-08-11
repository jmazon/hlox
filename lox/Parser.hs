module Parser where

import Prelude hiding (or,and)
import Data.Maybe
import Data.IORef
import Data.Unique
import Control.Monad
import Control.Monad.Cont
import Control.Monad.Loops
import Control.Exception

import Misc

import qualified TokenType as TT
import TokenType (TokenType)
import Token
import Expr
import Stmt

data Parser = Parser {
    parserTokens :: [Token]
  , parserCurrent :: IORef Int
  }

newParser :: [Token] -> IO Parser
newParser tokens = do
  Parser tokens <$> newIORef 0

parse :: Parser -> IO [Stmt]
parse p = do
  fmap catMaybes $ whileM (not <$> isAtEnd p) $ declaration p

expression :: Parser -> IO Expr
expression = assignment

declaration :: Parser -> IO (Maybe Stmt)
declaration p = do
  handle (\ParseError -> synchronize p >> return Nothing) $ Just <$> caseM
    [ (match p [TT.Class],classDeclaration p)
    , (match p [TT.Fun],function p "function")
    , (match p [TT.Var],varDeclaration p) ]
    (statement p)

classDeclaration :: Parser -> IO Stmt
classDeclaration p = do
  name <- consume p TT.Identifier "Expect class name."
  consume p TT.LeftBrace "Expect '{' before class body."
  methods <- whileM (andM [not <$> check p TT.RightBrace,not <$> isAtEnd p]) $
               function p "method"
  consume p TT.RightBrace "Expect '}' after class body."
  return (Class name methods)

statement :: Parser -> IO Stmt
statement p = caseM
  [ (match p [TT.For],forStatement p)
  , (match p [TT.If],ifStatement p)
  , (match p [TT.Print],printStatement p)
  , (match p [TT.Return],returnStatement p)
  , (match p [TT.While],whileStatement p)
  , (match p [TT.LeftBrace],Block <$> block p)
  ]
  (expressionStatement p)

forStatement :: Parser -> IO Stmt
forStatement p = do
  consume p TT.LeftParen "Expect '(' after 'for'."
  initializer <- ifM (match p [TT.Semicolon]) (return Nothing) $
                 ifM (match p [TT.Var]) (Just <$> varDeclaration p) $
                 Just <$> expressionStatement p
  condition <- ifM (not <$> check p TT.Semicolon) (Just <$> expression p)
                                                  (return Nothing)
  consume p TT.Semicolon "Expect ';' after loop confition."
  increment <- ifM (not <$> check p TT.RightParen) (Just <$> expression p)
                                                   (return Nothing)
  consume p TT.RightParen "Expect ')' after for clauses."
  body <- statement p

  let body' = maybe body (\i -> Block [body,Expression i]) increment
      condition' = fromMaybe (Literal $ LBool True) condition
      body'' = While condition' body'
      body''' = maybe body'' (Block . (: [body''])) initializer

  return body'''
  
ifStatement :: Parser -> IO Stmt
ifStatement p = do
  consume p TT.LeftParen "Expect '(' after 'if'."
  condition <- expression p
  consume p TT.RightParen "Expect ')' after if condition."

  thenBranch <- statement p
  e <- match p [TT.Else]
  elseBranch <- if e then Just <$> statement p else return Nothing

  return (If condition thenBranch elseBranch)
  
printStatement :: Parser -> IO Stmt
printStatement p = do
  value <- expression p
  consume p TT.Semicolon "Expect ';' after value."
  return (Print value)

returnStatement :: Parser -> IO Stmt
returnStatement p = do
  keyword <- previous p
  value <- ifM (not <$> check p TT.Semicolon) (Just <$> expression p) (return Nothing)
  consume p TT.Semicolon "Expecct ';' after return value."
  return (Return keyword value)

varDeclaration :: Parser -> IO Stmt
varDeclaration p = do
  name <- consume p TT.Identifier "Expect variable name."
  eq <- match p [TT.Equal]
  initializer <- if eq then Just <$> expression p else return Nothing
  consume p TT.Semicolon "Expect ';' after variable declaration."
  return (Var name initializer)

whileStatement :: Parser -> IO Stmt
whileStatement p = do
  consume p TT.LeftParen "Expect '(' after 'while'."
  condition <- expression p
  consume p TT.RightParen "Expect ')' after condition."
  body <- statement p
  return (While condition body)
  
expressionStatement :: Parser -> IO Stmt
expressionStatement p = do
  expr <- expression p
  consume p TT.Semicolon "Expect ';' after expression."
  return (Expression expr)

function :: Parser -> String -> IO Stmt
function p kind = do
  name <- consume p TT.Identifier $ "Expect " ++ kind ++ " name."
  consume p TT.LeftParen $ "Expect '(' after " ++ kind ++ " name."

  let f i = do
        when (i == 255) $ void $ flip tokenError "Cannot have more than 255 parameters." =<< peek p
        ifM (match p [TT.Comma]) (Just . flip (,) (i+1) <$> c)
                                 (return Nothing)
      c = consume p TT.Identifier "Expect parameter name."
  parameters <- ifM (not <$> check p TT.RightParen)
                  (liftM2 (:) c (unfoldrM f 1)) (return [])

  consume p TT.RightParen "Expect ')' after parameters."

  consume p TT.LeftBrace $ "Expect '{' before " ++ kind ++ " body."
  body <- block p
  return (Function name parameters body)

block :: Parser -> IO [Stmt]
block p = do
  statements <- fmap catMaybes $
                whileM (andM [not <$> check p TT.RightBrace,not <$> isAtEnd p])
                  (declaration p)
  consume p TT.RightBrace "Expect '}' after block."
  return statements

assignment :: Parser -> IO Expr
assignment p = do
  expr <- or p
  eq <- match p [TT.Equal]
  if eq then do
      equals <- previous p
      value <- assignment p
      case expr of Variable name _ -> do
                     u <- newUnique
                     return (Assign name (Unique' u) value)
                   Get object name -> do
                     return (Set object name value)
                   _ -> tokenError equals "Invalid assignment target."
                        >> return (Literal LNull)
    else return expr

or,and :: Parser -> IO Expr
or p = leftAssoc p [TT.Or] Logical and
and p = leftAssoc p [TT.And] Logical equality

equality :: Parser -> IO Expr
equality p = binary p [TT.BangEqual,TT.EqualEqual] comparison

comparison :: Parser -> IO Expr
comparison p = binary p [TT.Greater,TT.GreaterEqual,TT.Less,TT.LessEqual]
                        addition

addition :: Parser -> IO Expr
addition p = binary p [TT.Minus,TT.Plus] multiplication

multiplication :: Parser -> IO Expr
multiplication p = binary p [TT.Slash,TT.Star] unary

unary :: Parser -> IO Expr
unary p = do
  m <- match p [TT.Bang,TT.Minus]
  if m then do
    operator <- previous p
    right <- unary p
    return $ Unary operator right
    else call p

finishCall :: Parser -> Expr -> IO Expr
finishCall p callee = do
  let f i = do
        when (i == 255) $ void $ flip tokenError "Cannot have more than 255 arguments." =<< peek p
        ifM (match p [TT.Comma]) (Just . flip (,) (i+1) <$> expression p)
                                 (return Nothing)
  arguments <- ifM (not <$> check p TT.RightParen)
                 (liftM2 (:) (expression p) (unfoldrM f 1))
                 (return [])
  paren <- consume p TT.RightParen "Expect ')' after arguments."
  return (Call callee paren arguments)

call :: Parser -> IO Expr
call p = flip runContT readIORef $ do
  expr <- lift (newIORef =<< primary p)
  callCC $ \exit -> forever $ do
    caseM [ (lift $ match p [TT.LeftParen]
            ,lift $ writeIORef expr =<< finishCall p =<< readIORef expr)
          , (lift $ match p [TT.Dot]
            ,lift $ do name <- consume p TT.Identifier "Expect property name after '.'."
                       writeIORef expr . flip Get name =<< readIORef expr) ]
      (exit ())
  return expr

primary :: Parser -> IO Expr
primary p = caseM
  [ (match p [TT.False],return (Literal (LBool False)))
  , (match p [TT.True],return (Literal (LBool True)))
  , (match p [TT.Nil],return (Literal (LNull)))
  , (match p [TT.Number],Literal . tokenLiteral <$> previous p)
  , (match p [TT.String],Literal . tokenLiteral <$> previous p)
  , (match p [TT.This],liftM2 This (previous p) (Unique' <$> newUnique))
  , (match p [TT.Identifier],liftM2 Variable (previous p) (Unique' <$> newUnique))
  , (match p [TT.LeftParen],do
        expr <- expression p
        consume p TT.RightParen "Expect ')' after expression."
        return (Grouping expr)) ]
  (throwIO =<< flip tokenError "Expect expression." =<< peek p)

caseM :: Monad m => [(m Bool,m a)] -> m a -> m a
caseM cs def = go cs where
  go ((p,b):cs) = do p <- p
                     if p then b else go cs
  go [] = def
  
binary :: Parser -> [TokenType] -> (Parser -> IO Expr) -> IO Expr
binary p tokens next = leftAssoc p tokens Binary next

leftAssoc :: Parser -> [TokenType] -> (Expr -> Token -> Expr -> Expr) -> (Parser -> IO Expr) -> IO Expr
leftAssoc p tokens node next = do
  expr <- next p
  fmap (foldl (\l (o,r) -> node l o r) expr) $
    whileM (match p tokens) $ do
      operator <- previous p
      right <- next p
      return (operator,right)

match :: Parser -> [TokenType] -> IO Bool
match p = anyM f where
  f t = do c <- check p t
           when c $ void $ advance p
           return c

consume :: Parser -> TokenType -> String -> IO Token
consume p tt msg = do
  c <- check p tt
  if c then void (advance p) else throwIO =<< flip tokenError msg =<< peek p
  previous p

check :: Parser -> TokenType -> IO Bool
check p tt = do
  ae <- isAtEnd p
  if ae then return False else (== tt) . tokenType <$> peek p

advance :: Parser -> IO Token
advance p = do
  ae <- isAtEnd p
  unless ae $ modifyIORef (parserCurrent p) succ
  previous p

isAtEnd :: Parser -> IO Bool
isAtEnd p = (== TT.Eof) . tokenType <$> peek p

peek :: Parser -> IO Token
peek p = (parserTokens p !!) <$> readIORef (parserCurrent p)

previous :: Parser -> IO Token
previous p = (parserTokens p !!) . pred <$> readIORef (parserCurrent p)

synchronize :: Parser -> IO ()
synchronize p = do
  advance p
  whileM_ (andM [ not <$> isAtEnd p
                , (/= TT.Semicolon) . tokenType <$> previous p
                , (`notElem` [TT.Class,TT.Fun,TT.Var,TT.For
                             ,TT.If,TT.While,TT.Print,TT.Return]) . tokenType
                  <$> peek p ] ) $
    advance p

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM c t e = do c >>= \p -> if p then t else e
