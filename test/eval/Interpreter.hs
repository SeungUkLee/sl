{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Interpreter where

import           Control.Monad.Except
import           SLang

newtype TestEval a = TestEval
  { runTestEval :: Except EvalError a
  } deriving ( Monad
             , Applicative
             , Functor
             , MonadError EvalError
             )

instance SLangEval TestEval where
  evaluate expr = do
    eitherResult <- runSLangEval expr
    case eitherResult of
      Left err  -> throwError err
      Right val -> return val
