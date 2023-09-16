{-# LANGUAGE OverloadedStrings #-}

module SLang.TypeInfer.Error
  ( TypeError (..)
  ) where

import           Control.Exception    (Exception (displayException))
import qualified Data.Text            as T
import           SLang.Eval.Syntax    (Expr)
import qualified SLang.Pretty         as SP
import           SLang.TypeInfer.Type (Type)

data TypeError
  = UnificationError Type Type Expr
  | UnboundVar T.Text
  deriving Show

instance Exception TypeError where
  displayException = T.unpack . displayTypeError

displayTypeError :: TypeError -> T.Text
displayTypeError (UnboundVar name) =
  T.concat ["[Error] unbound variable : ", name, "\n" ]

displayTypeError (UnificationError received expected expr) =
  T.concat
    [ "[Error] \"",  SP.toText $ SP.pretty expr
    , "\" expression has type \"", SP.toText $ SP.pretty received
    , "\" but an expression was expected of type \"", SP.toText $ SP.pretty expected, "\"\n"
    , "  expected: ", SP.toText $ SP.pretty expected, "\n"
    , "    actual: ", SP.toText $ SP.pretty received, "\n"
    ]
