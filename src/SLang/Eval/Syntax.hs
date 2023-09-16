module SLang.Eval.Syntax
  ( Expr (..)
  , Const (..)
  , Bop (..)
  , Name
  , LetBind (..)
  ) where

import qualified Data.Text     as T
import           Prettyprinter (Doc, (<+>))
import qualified SLang.Pretty  as SP
import           SLang.Pretty  (Pretty (pretty))

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

type Name = T.Text

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

instance Pretty Expr where
  pretty = pprExpr

pprExpr :: Expr -> Doc ann
pprExpr (EConst c) = pprConst c
pprExpr (EVar name) = SP.pretty name
pprExpr (EApp func arg) = SP.parensIf (isAbs func) (pprExpr func)  <+> pprExpr arg
pprExpr (EAbs name body) = SP.pretty "fun" <+> SP.pretty name <+> SP.pretty "->" <+> pprExpr body
pprExpr (ELet bind body) = SP.pretty "let" <+> pprLetBind bind <+> SP.pretty "in" <+> pprExpr body
pprExpr (EIf cond th el) = SP.pretty "if" <+> pprExpr cond <+> SP.pretty "then" <+> pprExpr th <+> SP.pretty "else" <+> pprExpr el
pprExpr (EOp bop e1 e2) = pprExpr e1 <+> pprBop bop <+> pprExpr e2

isAbs :: Expr -> Bool
isAbs EAbs{} = True
isAbs _      = False

pprLetBind :: LetBind -> Doc ann
pprLetBind (LBRec fnName argName evalue) = SP.pretty "rec" <+> SP.pretty fnName <+> SP.pretty argName <+> SP.pretty "=" <+> pprExpr evalue
pprLetBind (LBVal name evalue) = SP.pretty name <+> SP.pretty "=" <+> pprExpr evalue

pprBop :: Bop -> Doc ann
pprBop Add   = SP.pretty "+"
pprBop Sub   = SP.pretty "-"
pprBop Mul   = SP.pretty "*"
pprBop Equal = SP.pretty "=="

pprConst :: Const -> Doc ann
pprConst (CInt n)      = SP.pretty n
pprConst (CBool True)  = SP.pretty "true"
pprConst (CBool False) = SP.pretty "false"
