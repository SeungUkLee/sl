{-# LANGUAGE OverloadedStrings #-}

module SLang.Parser
  ( parseToExpr
  , ParseError
  ) where

import           Text.Megaparsec                (MonadParsec (eof, try), choice,
                                                 many, runParser, some, (<|>))

import           Control.Monad.Combinators.Expr (makeExprParser)

import qualified Data.Text                      as T
import           SLang.Eval.Syntax              (Const (..), Expr (..),
                                                 LetBind (..))
import           SLang.Parser.Common            (Parser)
import           SLang.Parser.Error             (ParseError (..))
import           SLang.Parser.Lexer             (identifier, operatorTable,
                                                 parens, reserved, sc,
                                                 signedInteger, symbol)

pTerm :: Parser Expr
pTerm = choice
  [ parens pExpr
  , pVariable
  , pInteger
  , pBool
  , pIf
  , pFunc
  , try pLetRec
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
  ELet (LBVal name (foldr EAbs evalue args)) <$> pExpr

pLetRec :: Parser Expr
pLetRec = do
  reserved "let"
  reserved "rec"
  fNname <- identifier
  args <- many identifier
  _ <- symbol "="
  evalue <- pExpr
  reserved "in"
  case pm args evalue of
    ("", args', _) -> ELet (LBVal fNname (foldr EAbs evalue args')) <$> pExpr
    (argName, args', evalue') -> ELet (LBRec fNname argName (foldr EAbs evalue' args')) <$> pExpr
  where
    -- | TODO: rename ...
    pm :: [String] -> Expr -> (String, [String], Expr)
    pm li e = case (li, e) of
      ([], EAbs name e') -> (name, [], e')
      ([], _)            -> ("", [], e)
      (x:xs, _)          -> (x, xs, e)

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

parseToExpr :: FilePath -> T.Text -> Either ParseError Expr
parseToExpr file txt = case runParser (sc *> pExpr <* eof) file (T.strip txt) of
  Left e     -> Left $ ParseError e
  Right expr -> Right expr
