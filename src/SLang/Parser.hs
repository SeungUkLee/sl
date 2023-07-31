{-# LANGUAGE OverloadedStrings #-}

module SLang.Parser
  ( parseSL
  ) where

import           Text.Megaparsec                (MonadParsec (eof), choice,
                                                 many, runParser, some, (<|>))

import           Control.Monad.Combinators.Expr (makeExprParser)

import           SLang.Eval.Syntax              (Const (..), Expr (..))
import           SLang.Parser.Common            (Parser)
import           SLang.Parser.Lexer             (identifier, operatorTable,
                                                 parens, reserved,
                                                 signedInteger, symbol, sc)

pTerm :: Parser Expr
pTerm = choice
  [ parens pExpr
  , pVariable
  , pInteger
  , pBool
  , pIf
  , pFunc
  , pLet
  ]

pExpr :: Parser Expr
pExpr = do
  ts <- some expr
  return (foldl1 EApp ts)
  where
    expr = makeExprParser pTerm operatorTable

pVariable :: Parser Expr
pVariable = EVar <$> identifier

pInteger :: Parser Expr
pInteger = EConst . CInt <$> signedInteger

pBool :: Parser Expr
pBool = EConst <$> (CBool True <$ reserved "true" <|> CBool False <$ reserved "false")

pLet :: Parser Expr
pLet = do
  reserved "let"
  name <- identifier
  args <- many identifier
  _ <- symbol "="
  evalue <- pExpr
  reserved "in"
  ELet name (foldr EAbs evalue args) <$> pExpr

pIf :: Parser Expr
pIf = do
  reserved "if"
  cond <- pExpr
  reserved "then"
  thn <- pExpr
  reserved "else"
  EIf cond thn <$> pExpr

pFunc :: Parser Expr
pFunc = do
  reserved "fun"
  args <- many identifier
  _ <- symbol "->"
  body <- pExpr
  return (foldr EAbs body args)

parseSL = runParser (sc *> pExpr <* eof) "sl parser"
