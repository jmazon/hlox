{-# LANGUAGE TupleSections,ScopedTypeVariables,OverloadedStrings #-}
module Parser (parse) where

import Prelude hiding (or,and)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.DList as D
import Control.Monad.Writer.Strict
import Control.Monad.State.Strict
import Control.Monad.Except
import Data.Either
import Data.Dynamic

import Util
import qualified TokenType as TT
import TokenType (TokenType)
import Token (Token,tokenLiteral,tokenType)
import Expr
import Stmt

data ParseError = ParseError Text Token

data ParserState = ParserState { tokens     :: !(DList Token)
                               , nextUnique :: !ExprKey }

type MP = ExceptT ParseError (StateT ParserState (Writer (DList (Token,Text))))

parse :: DList Token -> ([Stmt],DList (Token,Text))
parse toks = runWriter $
  flip evalStateT (ParserState (cons (error "Read before token stream") toks)
                               (ExprKey 0)) $
  fmap (fromRight (error "Internal error: leaked ParseError")) $ runExceptT $
  fmap catMaybes $ whileM (not <$> isAtEnd) declaration
{-# ANN parse ( "HLint: ignore Use <$>" :: String ) #-}

expression :: MP Expr
expression = assignment

declaration :: MP (Maybe Stmt)
declaration =
  (Just <$> caseM [ match TT.Class classDeclaration
                  , match TT.Fun (Function <$> function "function")
                  , match TT.Var varDeclaration ]
              statement)
    `catchError`
  \(_ :: ParseError) -> do
    synchronize
    return Nothing

classDeclaration :: MP Stmt
classDeclaration = do
  name <- consume TT.Identifier "Expect class name."
  superclass <- match TT.Less $ do
    consume_ TT.Identifier "Expect superclass name."
    liftM2 Variable previous newUnique
  consume_ TT.LeftBrace "Expect '{' before class body."
  methods <- whileM (andM [not <$> check TT.RightBrace,not <$> isAtEnd]) $
               function "method"
  consume_ TT.RightBrace "Expect '}' after class body."
  return (Class name superclass methods)

statement :: MP Stmt
statement = caseM [ match TT.For forStatement
                  , match TT.If ifStatement
                  , match TT.Print printStatement
                  , match TT.Return returnStatement
                  , match TT.While whileStatement
                  , match TT.LeftBrace (Block <$> block) ]
              expressionStatement

forStatement :: MP Stmt
forStatement = do
  consume_ TT.LeftParen "Expect '(' after 'for'."
  initializer <- caseM [ match TT.Semicolon (return Nothing)
                       , match TT.Var (Just <$> varDeclaration) ]
                   (Just <$> expressionStatement)
  condition <- whenM' (not <$> check TT.Semicolon) expression
  consume_ TT.Semicolon "Expect ';' after loop condition."
  increment <- whenM' (not <$> check TT.RightParen) expression
  consume_ TT.RightParen "Expect ')' after for clauses."
  body <- statement

  let body' = maybe body (\i -> Block [body,Expression i]) increment
      condition' = fromMaybe (Literal $ toDyn True) condition
      body'' = While condition' body'
      body''' = maybe body'' (Block . (: [body''])) initializer

  return body'''
  
ifStatement :: MP Stmt
ifStatement = do
  consume_ TT.LeftParen "Expect '(' after 'if'."
  condition <- expression
  consume_ TT.RightParen "Expect ')' after if condition."

  thenBranch <- statement
  elseBranch <- match TT.Else statement

  return (If condition thenBranch elseBranch)
  
printStatement :: MP Stmt
printStatement = do
  value <- expression
  consume_ TT.Semicolon "Expect ';' after value."
  return (Print value)

returnStatement :: MP Stmt
returnStatement = do
  keyword <- previous
  value <- whenM' (not <$> check TT.Semicolon) expression
  consume_ TT.Semicolon "Expect ';' after return value."
  return (Return keyword value)

varDeclaration :: MP Stmt
varDeclaration = do
  name <- consume TT.Identifier "Expect variable name."
  initializer <- match TT.Equal expression
  consume_ TT.Semicolon "Expect ';' after variable declaration."
  return (Var name initializer)

whileStatement :: MP Stmt
whileStatement = do
  consume_ TT.LeftParen "Expect '(' after 'while'."
  condition <- expression
  consume_ TT.RightParen "Expect ')' after condition."
  body <- statement
  return (While condition body)
{-# ANN whileStatement ( "HLint: ignore Use <$>" :: String ) #-}
  
expressionStatement :: MP Stmt
expressionStatement = do
  expr <- expression
  consume_ TT.Semicolon "Expect ';' after expression."
  return (Expression expr)

function :: Text -> MP FunDecl
function kind = do
  name <- consume TT.Identifier (T.concat ["Expect ",kind," name."])
  consume_ TT.LeftParen (T.concat ["Expect '(' after ",kind," name."])

  let f (i::Int) = match TT.Comma $ do
        when (i >= 255) $ void $ parseError "Cannot have more than 255 parameters." =<< peek
        (, i+1) <$> c
      c = consume TT.Identifier "Expect parameter name."
  parameters <- ifM (not <$> check TT.RightParen)
                  (liftM2 (:) c (unfoldrM f 1)) (return [])

  consume_ TT.RightParen "Expect ')' after parameters."

  consume_ TT.LeftBrace (T.concat ["Expect '{' before ",kind," body."])
  body <- block
  return (FunDecl name parameters body)
{-# ANN function ( "HLint: ignore Use <$>" :: String ) #-}

block :: MP [Stmt]
block = do
  statements <- catMaybes <$>
                whileM (andM [not <$> check TT.RightBrace,not <$> isAtEnd])
                  declaration
  consume_ TT.RightBrace "Expect '}' after block."
  return statements

assignment :: MP Expr
assignment = do
  expr <- or
  fmap (fromMaybe expr) $ match TT.Equal $ do
      equals <- previous
      value <- assignment
      case expr of Variable name _ -> do
                     u <- newUnique
                     return (Assign name u value)
                   Get object name -> return (Set object name value)
                   _ -> parseError "Invalid assignment target." equals
                        >> return (Literal $ toDyn ())

or,and :: MP Expr
or = leftAssoc [logical TT.Or LogOr] and
and = leftAssoc [logical TT.And LogAnd] equality

logical :: TokenType -> LogicalOp -> (TokenType,Expr -> Token -> Expr -> Expr)
logical tt o = (tt,\l _ r -> Logical l o r)

equality :: MP Expr
equality = binary [ (TT.BangEqual,BinBangEqual)
                  , (TT.EqualEqual,BinEqualEqual) ] comparison

comparison :: MP Expr
comparison = binary [ (TT.Greater,BinGreater)
                    , (TT.GreaterEqual,BinGreaterEqual)
                    , (TT.Less,BinLess)
                    , (TT.LessEqual,BinLessEqual) ] addition

addition :: MP Expr
addition = binary [ (TT.Minus,BinMinus)
                  , (TT.Plus,BinPlus) ] multiplication

multiplication :: MP Expr
multiplication = binary [ (TT.Slash,BinSlash)
                        , (TT.Star,BinStar) ] unary

unary :: MP Expr
unary = caseM [ match TT.Bang  (liftM2 (Unary UnaryBang) previous unary)
              , match TT.Minus (liftM2 (Unary UnaryMinus) previous unary) ]
          call

finishCall :: Expr -> MP Expr
finishCall callee = do
  let f (i::Int) = match TT.Comma $ do
        when (i >= 255) $ void $ parseError "Cannot have more than 255 arguments." =<< peek
        (, i+1) <$> expression
  arguments <- ifM (not <$> check TT.RightParen)
                 (liftM2 (:) expression (unfoldrM f 1))
                 (return [])
  paren <- consume TT.RightParen "Expect ')' after arguments."
  return (Call callee paren arguments)

call :: MP Expr
call = do
  expr <- primary
  flip fix expr $ \f left ->
    caseM [ match TT.LeftParen (f =<< finishCall left)
          , match TT.Dot $ do
              name <- consume TT.Identifier "Expect property name after '.'."
              f (Get left name) ]
      (return left)

primary :: MP Expr
primary = caseM
  [ match TT.False (return (Literal (toDyn False)))
  , match TT.True (return (Literal (toDyn True)))
  , match TT.Nil (return (Literal (toDyn ())))
  , match TT.Number (Literal . tokenLiteral <$> previous)
  , match TT.String (Literal . tokenLiteral <$> previous)
  , match TT.Super $ do
        keyword <- previous
        consume_ TT.Dot "Expect '.' after 'super'."
        method <- consume TT.Identifier "Expect superclass method name."
        key <- newUnique
        return (Super keyword key method)
  , match TT.This (liftM2 This previous newUnique)
  , match TT.Identifier (liftM2 Variable previous newUnique)
  , match TT.LeftParen $ do
        expr <- expression
        consume_ TT.RightParen "Expect ')' after expression."
        return (Grouping expr) ]
  (throwError =<< parseError "Expect expression." =<< peek)
  
binary ::  [(TokenType,BinaryOp)] -> MP Expr -> MP Expr
binary tokops next = leftAssoc (fmap bin tokops) next where
  bin (tt,bo) = (tt,\l t r -> Binary l t bo r)
{-# ANN binary ("HLint: ignore Eta reduce" :: String) #-}

leftAssoc :: [(TokenType,Expr -> Token -> Expr -> Expr)] -> MP Expr -> MP Expr
leftAssoc tokops next = do
  expr <- next
  flip fix expr $ \f left ->
    caseM ((\(t,o) -> match t (do operator <- previous
                                  right <- next
                                  f (o left operator right))) <$> tokops)
      (return left)

match :: TokenType -> MP a -> MP (Maybe a)
match t body = do
  c <- check t
  if c then advance_ >> Just <$> body else return Nothing

consume_ :: TokenType -> Text -> MP ()
consume_ tt msg = do
  c <- check tt
  if c then advance_ else throwError =<< parseError msg =<< peek
consume :: TokenType -> Text -> MP Token
consume tt msg = do
  consume_ tt msg
  previous

check :: TokenType -> MP Bool
check tt = do
  ae <- isAtEnd
  if ae then return False else (== tt) . tokenType <$> peek

advance_ :: MP ()
advance_ = do
  ae <- isAtEnd
  unless ae $ modify (\s -> s { tokens = D.tail (tokens s)})

-- XXX advance is never used
advance :: MP Token
advance = do
  advance_
  previous

isAtEnd :: MP Bool
isAtEnd = (== TT.Eof) . tokenType <$> peek

peek :: MP Token
peek = gets (D.head . D.tail . tokens)

previous :: MP Token
previous = gets (D.head . tokens)

parseError :: Text -> Token -> MP ParseError
parseError msg tok = do
  tell (singleton (tok,msg))
  return (ParseError msg tok)

synchronize :: MP ()
synchronize = do
  advance_
  whileM_ (andM [ not <$> isAtEnd
                , (/= TT.Semicolon) . tokenType <$> previous
                , (`notElem` [TT.Class,TT.Fun,TT.Var,TT.For
                             ,TT.If,TT.While,TT.Print,TT.Return]) . tokenType
                  <$> peek ] )
    advance_

newUnique :: MP ExprKey
newUnique = do
  r@(ExprKey u) <- gets nextUnique
  modify (\s -> s { nextUnique = ExprKey (u + 1) })
  return r
