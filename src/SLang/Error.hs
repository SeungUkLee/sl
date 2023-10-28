module SLang.Error
  ( SLangError (..)
  ) where

import           Control.Monad.Catch
import           SLang.Eval
import           SLang.Parser
import           SLang.TypeInfer

data SLangError
  = ParserError ParseError
  | EvaluatorError EvalError
  | TypeInferError TypeError
  deriving Show

instance Exception SLangError where
  displayException (ParserError e)    = displayException e
  displayException (EvaluatorError e) = displayException e
  displayException (TypeInferError e) = displayException e
