module SLang.Eval.Error
  ( EvalError (..)
  ) where

import qualified Data.Text as T

data EvalError
  = TypeMissmatch T.Text
  | UnboundVariable T.Text

instance Show EvalError where
  show (TypeMissmatch txt)    = "[Error] type missmatch : " ++ T.unpack txt
  show (UnboundVariable name) = "[Error] unbound variable : " ++ T.unpack name
