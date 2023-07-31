module SLang.TypeInfer.Error
  ( TypeError (..)
  ) where

import           SLang.TypeInfer.Type (Type)

data TypeError
  = UnificationError Type Type
  | UnboundVar String

instance Show TypeError where
  show (UnboundVar name) = "[Type error] unbound variable : " ++ name
  show (UnificationError received expected) =
    "[Type error] unification error: \n" ++ "expected: " ++ show expected ++  "\n" ++ "recevied: " ++ show received
