{-# LANGUAGE OverloadedStrings #-}

module SLang.Interative.Result
  ( Result (..)
  ) where

import           Prettyprinter        ((<+>))

import qualified Data.Text            as T
import           SLang.Eval.Domain    (Value)
import           SLang.Eval.Syntax    (Expr)
import qualified SLang.Pretty         as SP
import           SLang.Pretty         (Pretty (..))
import           SLang.TypeInfer.Type (Type)

data Result
  = Interpret Type Value
  | Parse Expr
  | TypeInfer Expr Type
  | Define T.Text
  | Load Type Value

instance Pretty Result where
  pretty (Interpret typ val)  = "-" <+> ":" <+> SP.pretty typ <+> "=" <+> SP.pretty val <> "\n"
  pretty (Parse ast)          = SP.pretty $ show ast <> "\n"
  pretty (TypeInfer expr typ) = SP.pretty expr <+> ":" <+> SP.pretty typ <> "\n"
  pretty (Define txt)         = SP.pretty txt
  pretty (Load typ val)       = "-" <+> ":" <+> SP.pretty typ <+> "=" <+> SP.pretty val <> "\n"
