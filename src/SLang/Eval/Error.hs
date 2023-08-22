module SLang.Eval.Error
  ( EvalError (..)
  ) where

data EvalError
  = TypeMissmatch String
  | UnboundVariable String

instance Show EvalError where
  show (TypeMissmatch txt)    = "[Error] type missmatch : " ++ txt
  show (UnboundVariable name) = "[Error] unbound variable : " ++ name
