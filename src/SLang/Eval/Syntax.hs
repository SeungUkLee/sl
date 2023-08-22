module SLang.Eval.Syntax
  ( Expr (..)
  , Const (..)
  , Bop (..)
  , Name
  , LetBind (..)
  , showStrExpr
  ) where

import           Prettyprinter (Doc, Pretty (pretty), (<+>))
import           SLang.Pretty  (parensIf)

data Expr
  = EConst Const
  | EVar Name
  | EApp Expr Expr
  | EAbs Name Expr
  | ELet LetBind Expr
  | EIf Expr Expr Expr
  | EOp Bop Expr Expr
  deriving Show

data LetBind
  = LBRec Name Name Expr
  | LBVal Name Expr
  deriving Show

type Name = String

data Const
  = CInt Integer
  | CBool Bool
  deriving Show

data Bop
  = Add
  | Sub
  | Mul
  | Equal
  deriving Show

showStrExpr :: Expr -> String
showStrExpr = show . pprExpr

pprExpr :: Expr -> Doc ann
pprExpr (EConst c) = pprConst c
pprExpr (EVar name) = pretty name
pprExpr (EApp func arg) = parensIf (isAbs func) (pprExpr func)  <+> pprExpr arg
pprExpr (EAbs name body) = pretty "fun" <+> pretty name <+> pretty "->" <+> pprExpr body
pprExpr (ELet bind body) = pretty "let" <+> pprLetBind bind <+> pretty "in" <+> pprExpr body
pprExpr (EIf cond th el) = pretty "if" <+> pprExpr cond <+> pretty "then" <+> pprExpr th <+> pretty "else" <+> pprExpr el
pprExpr (EOp bop e1 e2) = pprExpr e1 <+> pprBop bop <+> pprExpr e2
  
isAbs :: Expr -> Bool
isAbs EAbs{} = True
isAbs _      = False

pprLetBind :: LetBind -> Doc ann
pprLetBind (LBRec fnName argName evalue) = pretty "rec" <+> pretty fnName <+> pretty argName <+> pretty "=" <+> pprExpr evalue
pprLetBind (LBVal name evalue) = pretty name <+> pretty "=" <+> pprExpr evalue

pprBop :: Bop -> Doc ann
pprBop Add   = pretty "+"
pprBop Sub   = pretty "-"
pprBop Mul   = pretty "*"
pprBop Equal = pretty "=="

pprConst :: Const -> Doc ann
pprConst (CInt n)  = pretty n
pprConst (CBool True) = pretty "true"
pprConst (CBool False) = pretty "false"
