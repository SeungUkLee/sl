{-# LANGUAGE OverloadedStrings #-}

module SLang.Parser.Lexer
  ( sc
  , identifier
  , operatorTable
  , parens
  , reserved
  , symbol
  , signedInteger
  ) where

import           Data.Text                      (Text)
import           Text.Megaparsec                (MonadParsec (notFollowedBy, try),
                                                 between, many)
import           Text.Megaparsec.Char           (alphaNumChar, letterChar,
                                                 space1, string)
import qualified Text.Megaparsec.Char.Lexer     as L

import           Control.Monad.Combinators.Expr (Operator (..))

import           SLang.Eval.Syntax              (Bop (..), Expr (..))
import           SLang.Parser.Common            (Parser)

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "(* *)")
  (L.skipBlockComment "(*" "*)")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

reserved :: Text -> Parser ()
reserved w = (lexeme . try) $ string w *> notFollowedBy alphaNumChar

reservedWords :: [String]
reservedWords = ["if", "then", "else", "let", "rec", "fun", "in", "true", "false"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` reservedWords
              then fail $ "keyword " ++ show x ++ " cannot be an identifier"
              else return x

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [
    [ binary "*" (EOp Mul)
    ]
  , [ binary "+" (EOp Add)
    , binary "-" (EOp Sub)
    , binary "==" (EOp Equal)
    ]
  ]

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ symbol name)

-- prefix, postfix :: Text -> (Expr -> Expr) -> Operator Parser Expr
-- prefix  name f = Prefix (f <$ symbol name)
-- postfix name f = Postfix (f <$ symbol name)

signedInteger :: Parser Integer
signedInteger = L.signed sc (lexeme L.decimal)
