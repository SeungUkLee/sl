{-# LANGUAGE OverloadedStrings #-}

module SLang.Result (Result (..)) where

import           Prettyprinter        ((<+>))

import           SLang.Eval.Domain    (Value)
import           SLang.Eval.Syntax    (Expr)
import qualified SLang.Pretty         as SP
import           SLang.Pretty         (Pretty (..))
import           SLang.TypeInfer.Type (Type)

data Result
  = Interpret Type Value
  | Parse Expr
  | TypeInfer Expr Type

instance Pretty Result where
  pretty (Interpret typ val)  = "-" <+> ":" <+> SP.pretty typ <+> "=" <+> SP.pretty val <> "\n"
  pretty (Parse ast)          = SP.pretty $ show ast <> "\n"
  pretty (TypeInfer expr typ) = SP.pretty expr <+> ":" <+> SP.pretty typ <> "\n"
