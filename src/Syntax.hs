module Syntax
  ( Expr (..)
  , Const (..)
  , Bop (..)
  , Name
  ) where

data Expr
  = EConst Const
  | EVar Name
  | EApp Expr Expr
  | EAbs Name Expr
  | ELet Name Expr Expr
  | EIf Expr Expr Expr
  | EOp Bop Expr Expr
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
