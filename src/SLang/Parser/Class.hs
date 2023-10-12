module SLang.Parser.Class
  ( SLangParser (..)
  ) where

import qualified Data.Text         as T
import           SLang.Eval.Syntax (Expr)

class (Monad m) => SLangParser m where
  parse :: FilePath -> T.Text -> m Expr
