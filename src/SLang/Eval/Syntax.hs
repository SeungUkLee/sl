module SLang.Eval.Syntax
  ( Expr (..)
  , Const (..)
  , Bop (..)
  , Name
  , LetBind (..)
  ) where

import           Prettyprinter (Doc, Pretty (pretty), (<+>))

data Expr
  = EConst Const
  | EVar Name
  | EApp Expr Expr
  | EAbs Name Expr
  | ELet LetBind Expr
  | EIf Expr Expr Expr
  | EOp Bop Expr Expr

data LetBind
  = LBRec Name Name Expr
  | LBVal Name Expr

type Name = String

data Const
  = CInt Integer
  | CBool Bool

data Bop
  = Add
  | Sub
  | Mul
  | Equal

instance Show Expr where
  show = show . pprExpr

instance Show Bop where
  show = show . pprBop

instance Show Const where
  show = show . pprConst

pprExpr :: Expr -> Doc ann
pprExpr (EConst c) = pprConst c
pprExpr (EVar name) = pretty name
pprExpr (EApp func arg) = pprExpr func  <+> pprExpr arg
pprExpr (EAbs name body) = pretty "fun" <+> pretty name <+> pprExpr body
pprExpr (ELet bind body) = pretty "let" <+> pprLetBind bind <+> pprExpr body
pprExpr (EIf cond th el) = pretty "if" <+> pprExpr cond <+> pretty "then" <+> pprExpr th <+> pretty "else" <+> pprExpr el
pprExpr (EOp bop e1 e2) = pprExpr e1 <+> pprBop bop <+> pprExpr e2

pprLetBind :: LetBind -> Doc ann
pprLetBind (LBRec fnName argName evalue) = pretty "rec" <+> pretty fnName <+> pretty argName <+> pprExpr evalue
pprLetBind (LBVal name evalue) = pretty name <+> pprExpr evalue

pprBop :: Bop -> Doc ann
pprBop Add   = pretty "+"
pprBop Sub   = pretty "-"
pprBop Mul   = pretty "*"
pprBop Equal = pretty "=="

pprConst :: Const -> Doc ann
pprConst (CInt n)  = pretty n
pprConst (CBool b) = pretty b
