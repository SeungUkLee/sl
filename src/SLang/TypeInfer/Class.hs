module SLang.TypeInfer.Class
  (SLangTypeInfer (..)
  ) where
import           SLang.Eval.Syntax    (Expr)
import           SLang.TypeInfer.Type (Type)

class (Monad m) => SLangTypeInfer m where
  infer :: Expr -> m Type
