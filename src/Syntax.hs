module Syntax (Expr (..), Const (..), Bop (..), Name) where

data Expr
  = Const Const
  | Var Name
  | App Expr Expr
  | Abs Name Expr
  | Let Name Expr Expr
  | If Expr Expr Expr
  | Op Bop Expr Expr
  deriving Show

type Name = String

data Const
  = Int Integer
  | Bool Bool
  deriving Show

data Bop
  = Add
  | Sub
  | Mul
  | Equal
  deriving Show
