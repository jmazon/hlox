{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections,ScopedTypeVariables,OverloadedStrings #-}
module Parser (parse) where

import Prelude hiding (or,and)
import Data.Maybe
import Data.IORef
import Data.Unique
import Control.Monad.Cont
import Data.Text (Text)
import qualified Data.Text as T
import Data.DList
import Control.Monad.Writer
import Control.Monad.Except
import Data.Either

import Util
import qualified TokenType as TT
import TokenType (TokenType)
import Token (Token,tokenLiteral,tokenType,Literal(LNull,LBool))
import Expr
import Stmt

data ParseError = ParseError Text Token deriving Show

data Parser = Parser {
    parserTokens :: [Token]
  , parserCurrent :: IORef Int
  }

type ParserError = (Token,Text)
type MP = ExceptT ParseError (WriterT (DList ParserError) IO)

newParser :: [Token] -> IO Parser
newParser tokens = Parser tokens <$> newIORef 0

parse :: [Token] -> IO ([Stmt],DList ParserError)
parse tokens = do
  p <- newParser tokens
  runWriterT $
    fmap (fromRight (error "Internal error: leaked ParseError")) $ runExceptT $
    fmap catMaybes $ whileM (not <$> isAtEnd p) $ declaration p

expression :: Parser -> MP Expr
expression = assignment

declaration :: Parser -> MP (Maybe Stmt)
declaration p =
  (Just <$> caseM [ (match p [TT.Class],classDeclaration p)
                  , (match p [TT.Fun],Function <$> function p "function")
                  , (match p [TT.Var],varDeclaration p) ]
              (statement p))
    `catchError`
  \(_ :: ParseError) -> do
    synchronize p
    return Nothing

classDeclaration :: Parser -> MP Stmt
classDeclaration p = do
  name <- consume p TT.Identifier "Expect class name."
  superclass <- ifM (match p [TT.Less]) (consume_ p TT.Identifier "Expect superclass name." >>
                                         Just <$> liftM2 Variable (previous p) (liftIO newUnique))
                      (return Nothing)
  consume_ p TT.LeftBrace "Expect '{' before class body."
  methods <- whileM (andM [not <$> check p TT.RightBrace,not <$> isAtEnd p]) $
               function p "method"
  consume_ p TT.RightBrace "Expect '}' after class body."
  return (Class name superclass methods)

statement :: Parser -> MP Stmt
statement p = caseM
  [ (match p [TT.For],forStatement p)
  , (match p [TT.If],ifStatement p)
  , (match p [TT.Print],printStatement p)
  , (match p [TT.Return],returnStatement p)
  , (match p [TT.While],whileStatement p)
  , (match p [TT.LeftBrace],Block <$> block p)
  ]
  (expressionStatement p)

forStatement :: Parser -> MP Stmt
forStatement p = do
  consume_ p TT.LeftParen "Expect '(' after 'for'."
  initializer <- ifM (match p [TT.Semicolon]) (return Nothing) $
                 ifM (match p [TT.Var]) (Just <$> varDeclaration p) $
                 Just <$> expressionStatement p
  condition <- ifM (not <$> check p TT.Semicolon) (Just <$> expression p)
                                                  (return Nothing)
  consume_ p TT.Semicolon "Expect ';' after loop condition."
  increment <- ifM (not <$> check p TT.RightParen) (Just <$> expression p)
                                                   (return Nothing)
  consume_ p TT.RightParen "Expect ')' after for clauses."
  body <- statement p

  let body' = maybe body (\i -> Block [body,Expression i]) increment
      condition' = fromMaybe (Literal $ LBool True) condition
      body'' = While condition' body'
      body''' = maybe body'' (Block . (: [body''])) initializer

  return body'''
  
ifStatement :: Parser -> MP Stmt
ifStatement p = do
  consume_ p TT.LeftParen "Expect '(' after 'if'."
  condition <- expression p
  consume_ p TT.RightParen "Expect ')' after if condition."

  thenBranch <- statement p
  e <- match p [TT.Else]
  elseBranch <- if e then Just <$> statement p else return Nothing

  return (If condition thenBranch elseBranch)
  
printStatement :: Parser -> MP Stmt
printStatement p = do
  value <- expression p
  consume_ p TT.Semicolon "Expect ';' after value."
  return (Print value)

returnStatement :: Parser -> MP Stmt
returnStatement p = do
  keyword <- previous p
  value <- ifM (not <$> check p TT.Semicolon) (Just <$> expression p) (return Nothing)
  consume_ p TT.Semicolon "Expect ';' after return value."
  return (Return keyword value)

varDeclaration :: Parser -> MP Stmt
varDeclaration p = do
  name <- consume p TT.Identifier "Expect variable name."
  eq <- match p [TT.Equal]
  initializer <- if eq then Just <$> expression p else return Nothing
  consume_ p TT.Semicolon "Expect ';' after variable declaration."
  return (Var name initializer)

whileStatement :: Parser -> MP Stmt
whileStatement p = do
  consume_ p TT.LeftParen "Expect '(' after 'while'."
  condition <- expression p
  consume_ p TT.RightParen "Expect ')' after condition."
  body <- statement p
  return (While condition body)
  
expressionStatement :: Parser -> MP Stmt
expressionStatement p = do
  expr <- expression p
  consume_ p TT.Semicolon "Expect ';' after expression."
  return (Expression expr)

function :: Parser -> Text -> MP FunDecl
function p kind = do
  name <- consume p TT.Identifier (T.concat ["Expect ",kind," name."])
  consume_ p TT.LeftParen (T.concat ["Expect '(' after ",kind," name."])

  let f (i::Int) = flip (ifM (match p [TT.Comma])) (return Nothing) $ do
        when (i >= 255) $ void $ parseError "Cannot have more than 255 parameters." =<< peek p
        Just . (, i+1) <$> c
      c = consume p TT.Identifier "Expect parameter name."
  parameters <- ifM (not <$> check p TT.RightParen)
                  (liftM2 (:) c (unfoldrM f 1)) (return [])

  consume_ p TT.RightParen "Expect ')' after parameters."

  consume_ p TT.LeftBrace (T.concat ["Expect '{' before ",kind," body."])
  body <- block p
  return (FunDecl name parameters body)

block :: Parser -> MP [Stmt]
block p = do
  statements <- catMaybes <$>
                whileM (andM [not <$> check p TT.RightBrace,not <$> isAtEnd p])
                  (declaration p)
  consume_ p TT.RightBrace "Expect '}' after block."
  return statements

assignment :: Parser -> MP Expr
assignment p = do
  expr <- or p
  eq <- match p [TT.Equal]
  if eq then do
      equals <- previous p
      value <- assignment p
      case expr of Variable name _ -> do
                     u <- liftIO newUnique
                     return (Assign name u value)
                   Get object name -> return (Set object name value)
                   _ -> parseError "Invalid assignment target." equals
                        >> return (Literal LNull)
    else return expr

or,and :: Parser -> MP Expr
or p = leftAssoc p [TT.Or] Logical and
and p = leftAssoc p [TT.And] Logical equality

equality :: Parser -> MP Expr
equality p = binary p [TT.BangEqual,TT.EqualEqual] comparison

comparison :: Parser -> MP Expr
comparison p = binary p [TT.Greater,TT.GreaterEqual,TT.Less,TT.LessEqual]
                        addition

addition :: Parser -> MP Expr
addition p = binary p [TT.Minus,TT.Plus] multiplication

multiplication :: Parser -> MP Expr
multiplication p = binary p [TT.Slash,TT.Star] unary

unary :: Parser -> MP Expr
unary p = do
  m <- match p [TT.Bang,TT.Minus]
  if m then do
    operator <- previous p
    right <- unary p
    return $ Unary operator right
    else call p

finishCall :: Parser -> Expr -> MP Expr
finishCall p callee = do
  let f (i::Int) = flip (ifM (match p [TT.Comma])) (return Nothing) $ do
        when (i >= 255) $ void $ parseError "Cannot have more than 255 arguments." =<< peek p
        Just . (, i+1) <$> expression p
  arguments <- ifM (not <$> check p TT.RightParen)
                 (liftM2 (:) (expression p) (unfoldrM f 1))
                 (return [])
  paren <- consume p TT.RightParen "Expect ')' after arguments."
  return (Call callee paren arguments)

call :: Parser -> MP Expr
call p = flip runContT (liftIO . readIORef) $ do
  expr <- liftIO . newIORef =<< lift (primary p)
  callCC $ \exit -> forever $
    caseM [ (lift $ match p [TT.LeftParen]
            ,lift $ liftIO . writeIORef expr =<< finishCall p =<< liftIO (readIORef expr))
          , (lift $ match p [TT.Dot]
            ,lift $ do name <- consume p TT.Identifier "Expect property name after '.'."
                       liftIO $ writeIORef expr . flip Get name =<< readIORef expr) ]
      (exit ())
  return expr

primary :: Parser -> MP Expr
primary p = caseM
  [ (match p [TT.False],return (Literal (LBool False)))
  , (match p [TT.True],return (Literal (LBool True)))
  , (match p [TT.Nil],return (Literal LNull))
  , (match p [TT.Number],Literal . tokenLiteral <$> previous p)
  , (match p [TT.String],Literal . tokenLiteral <$> previous p)
  , (match p [TT.Super],do
        keyword <- previous p
        consume_ p TT.Dot "Expect '.' after 'super'."
        method <- consume p TT.Identifier "Expect superclass method name."
        key <- liftIO newUnique
        return (Super keyword key method))
  , (match p [TT.This],liftM2 This (previous p) (liftIO newUnique))
  , (match p [TT.Identifier],liftM2 Variable (previous p) (liftIO newUnique))
  , (match p [TT.LeftParen],do
        expr <- expression p
        consume_ p TT.RightParen "Expect ')' after expression."
        return (Grouping expr)) ]
  (throwError =<< parseError "Expect expression." =<< peek p)
  
binary :: Parser -> [TokenType] -> (Parser -> MP Expr) -> MP Expr
binary p tokens next = leftAssoc p tokens Binary next
{-# ANN binary ("HLint: ignore Eta reduce" :: String) #-}

leftAssoc :: Parser -> [TokenType] -> (Expr -> Token -> Expr -> Expr) -> (Parser -> MP Expr) -> MP Expr
leftAssoc p tokens node next = do
  expr <- next p
  fmap (foldl (\l (o,r) -> node l o r) expr) $
    whileM (match p tokens) $ do
      operator <- previous p
      right <- next p
      return (operator,right)

match :: Parser -> [TokenType] -> MP Bool
match p = anyM f where
  f t = do c <- check p t
           when c $ advance_ p
           return c

consume_ :: Parser -> TokenType -> Text -> MP ()
consume_ p tt msg = do
  c <- check p tt
  if c then advance_ p else throwError =<< parseError msg =<< peek p
consume :: Parser -> TokenType -> Text -> MP Token
consume p tt msg = do
  consume_ p tt msg
  previous p

check :: Parser -> TokenType -> MP Bool
check p tt = do
  ae <- isAtEnd p
  if ae then return False else (== tt) . tokenType <$> peek p

advance_ :: Parser -> MP ()
advance_ p = do
  ae <- isAtEnd p
  unless ae $ liftIO $ modifyIORef (parserCurrent p) succ

-- XXX advance is never used
advance :: Parser -> MP Token
advance p = do
  advance_ p
  previous p

isAtEnd :: Parser -> MP Bool
isAtEnd p = (== TT.Eof) . tokenType <$> peek p

peek :: Parser -> MP Token
peek p = (parserTokens p !!) <$> liftIO (readIORef (parserCurrent p))

previous :: Parser -> MP Token
previous p = (parserTokens p !!) . pred <$> liftIO (readIORef (parserCurrent p))

parseError :: Text -> Token -> MP ParseError
parseError msg tok = do
  tell (singleton (tok,msg))
  return (ParseError msg tok)

synchronize :: Parser -> MP ()
synchronize p = do
  advance_ p
  whileM_ (andM [ not <$> isAtEnd p
                , (/= TT.Semicolon) . tokenType <$> previous p
                , (`notElem` [TT.Class,TT.Fun,TT.Var,TT.For
                             ,TT.If,TT.While,TT.Print,TT.Return]) . tokenType
                  <$> peek p ] ) $
    advance_ p
