{-# LANGUAGE OverloadedStrings #-}

module SLang.Eval.Error
  ( EvalError (..)
  ) where

import           Control.Exception (Exception (displayException))
import qualified Data.Text         as T

data EvalError
  = TypeMissmatch T.Text
  | UnboundVariable T.Text
  deriving (Show, Eq)

instance Exception EvalError where
  displayException = T.unpack . displayEvalError

displayEvalError :: EvalError -> T.Text
displayEvalError (TypeMissmatch txt) =
  T.concat ["[Error] type missmatch : ", txt, "\n"]
displayEvalError (UnboundVariable name) =
  T.concat ["[Error] unbound variable : ", name, "\n"]
