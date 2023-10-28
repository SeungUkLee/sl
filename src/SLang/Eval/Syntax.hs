{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module SLang.Eval.Syntax
  ( Expr (..)
  , Const (..)
  , Bop (..)
  , Name
  , LetBind (..)
  ) where

import qualified Data.Text            as T
import           Prettyprinter        (Doc, (<+>))
import qualified SLang.Pretty         as SP
import           SLang.Pretty         (Pretty (pretty))
import           SLang.TypeInfer.Type

data Expr
  = EConst Const
  | EVar Name
  | EApp Expr Expr
  | EAbs Name Expr
  | ELet LetBind Expr
  | EIf Expr Expr Expr
  | EOp Bop Expr Expr
  deriving (Show, Eq)

data LetBind
  = LBRec Name Name Expr
  | LBVal Name Expr
  deriving (Show, Eq)

type Name = T.Text

data Const
  = CInt Integer
  | CBool Bool
  deriving (Show, Eq)

data Bop
  = Add
  | Sub
  | Mul
  | Equal
  deriving (Show, Eq)

instance Pretty Expr where
  pretty = pprExpr

instance Pretty (Expr, Type) where
  pretty (expr, typ) = SP.pretty expr <+> ":" <+> SP.pretty typ <> "\n"

pprExpr :: Expr -> Doc ann
pprExpr (EConst c) = pprConst c
pprExpr (EVar name) = SP.pretty name
pprExpr (EApp func arg) = SP.parensIf (isAbs func) (pprExpr func)  <+> pprExpr arg
pprExpr (EAbs name body) = "fun" <+> SP.pretty name <+> "->" <+> pprExpr body
pprExpr (ELet bind body) = "let" <+> pprLetBind bind <+> "in" <+> pprExpr body
pprExpr (EIf cond th el) = "if" <+> pprExpr cond <+> "then" <+> pprExpr th <+> "else" <+> pprExpr el
pprExpr (EOp bop e1 e2) = pprExpr e1 <+> pprBop bop <+> pprExpr e2

isAbs :: Expr -> Bool
isAbs EAbs{} = True
isAbs _      = False

pprLetBind :: LetBind -> Doc ann
pprLetBind (LBRec fnName argName evalue) = "rec" <+> SP.pretty fnName <+> SP.pretty argName <+> "=" <+> pprExpr evalue
pprLetBind (LBVal name evalue) = SP.pretty name <+> "=" <+> pprExpr evalue

pprBop :: Bop -> Doc ann
pprBop Add   = "+"
pprBop Sub   = "-"
pprBop Mul   = "*"
pprBop Equal = "=="

pprConst :: Const -> Doc ann
pprConst (CInt n)      = SP.pretty n
pprConst (CBool True)  = "true"
pprConst (CBool False) = "false"
