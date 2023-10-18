module SLang.TypeInfer.Class
  ( SLangTypeInfer (..)
  ) where

import           SLang.Eval.Syntax    (Expr)
import           SLang.TypeInfer.Type (Type)

class SLangTypeInfer m where
  infer :: (Expr -> m Type) -> Expr -> m Type

  algorithmM :: Expr -> m Type
  algorithmW :: Expr -> m Type
