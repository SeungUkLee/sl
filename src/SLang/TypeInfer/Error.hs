module SLang.TypeInfer.Error
  ( TypeError (..)
  ) where

import           SLang.Eval.Syntax    (Expr, showStrExpr)
import           SLang.TypeInfer.Type (Type)

data TypeError
  = UnificationError Type Type Expr
  | UnboundVar String

instance Show TypeError where
  show (UnboundVar name) = "[Error] unbound variable : " ++ name ++ "\n"
  show (UnificationError received expected expr) =
    "[Error] \"" ++ showStrExpr expr ++ "\" expression has type \"" ++ show received
    ++ "\" but an expression was expected of type \"" ++ show expected ++ "\"\n"
    ++ "  expected: " ++ show expected ++  "\n"
    ++ "    actual: " ++ show received ++ "\n"
