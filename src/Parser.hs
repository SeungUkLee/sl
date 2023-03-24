{-# LANGUAGE OverloadedStrings #-}

module Parser where

import           Data.Text                      (Text)
import           Data.Void                      (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L

import           Control.Monad.Combinators.Expr

import           Syntax
import           Text.Megaparsec.Debug          (dbg)

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc


parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")


pTerm :: Parser Expr
--pTerm = do
--  ts <- some t
--  return (foldl1 App ts)
--  where
--    t = choice
--      [ parens pExpr
--      , pVariable
--      , pInteger
--      , pBool
--      , pIf
--      , pFunc
--      , pLet
--      ]
-- pTerm = makeExprParser t operatorTable
--   where
--     t = choice
--       [ parens pExpr
--       , pVariable
--       , pInteger
--       , pBool
--       , pIf
--       , pFunc
--       , pLet
--       ]
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
-- pExpr = makeExprParser pTerm operatorTable
pExpr = do
  ts <- some expr
  return (foldl1 App ts)
  where
    expr = makeExprParser pTerm operatorTable

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [
    [ binary "*" (Op Mul)
    ]
  , [ binary "+" (Op Add)
    , binary "-" (Op Sub)
    , binary "=" (Op Equal)
    ]
  ]

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ symbol name)

prefix, postfix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix  name f = Prefix (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)

reserved :: Text -> Parser ()
reserved w = (lexeme . try) $ string w *> notFollowedBy alphaNumChar

reservedWords :: [String]
reservedWords = ["if", "then", "else", "let", "fun", "in", "true", "false"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` reservedWords
              then fail $ "keyword " ++ show x ++ " cannot be an identifier"
              else return x

pVariable :: Parser Expr
-- pVariable = Var <$> lexeme ((:) <$> letterChar <*> many alphaNumChar <?> "variable")
pVariable = Var <$> identifier

pInteger :: Parser Expr
pInteger = Const . Int <$> signedInteger
-- pInteger = Const . Int <$> integer

-- integer :: Parser Integer
-- integer = lexeme L.decimal

signedInteger :: Parser Integer
signedInteger = L.signed sc (lexeme L.decimal)

pBool :: Parser Expr
pBool = Const <$> (Bool True <$ reserved "true" <|> Bool False <$ reserved "false")

pLet :: Parser Expr
pLet = do
  reserved "let"
  name <- identifier
  _ <- symbol "="
  evalue <- pExpr
  reserved "in"
  Let name evalue <$> pExpr

pIf :: Parser Expr
pIf = do
  reserved "if"
  cond <- pExpr
  reserved "then"
  thn <- pExpr
  reserved "else"
  If cond thn <$> pExpr

pFunc :: Parser Expr
pFunc = do
  reserved "fun"
  arg <- identifier
  _ <- symbol "->"
  Abs arg <$> pExpr

parseExpr :: Text -> IO ()
parseExpr = parseTest (pExpr <* eof)


-- run = runParser (pExpr <* eof) ""

-- testParse :: Parser a -> Text -> IO ()
-- testParse p = parseTest (p <* eof)

-- expr' :: Parser Expr
-- expr' = do
--   es <- some term'
--   return (foldl1 App es)

-- term' :: Parser Expr
-- term' = makeExprParser aexp' operatorTable

-- aexp' :: Parser Expr
-- aexp' = choice
--   [ parens expr'
--   , pVariable
--   , pInteger
--   , pBool
--   , pIf
--   , pFunc
--   , pLet
--   -- , parens pExpr
--   ]

-- parseExpr' :: Text -> IO ()
-- parseExpr' = parseTest (expr' <* eof)
