module SLang.Eval.Class
  ( SLangEval (..)
  ) where

import           SLang.Eval.Domain (Value)
import           SLang.Eval.Syntax (Expr)

class (Monad m) => SLangEval m where
  evaluate :: Expr -> m Value
