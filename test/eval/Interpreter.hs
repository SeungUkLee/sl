{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Interpreter 
  ( TestEval (..)
  ) where

import           Control.Monad.Except
import           SLang

newtype TestEval a = TestEval
  { runTestEval :: Except EvalError a
  } deriving ( Monad
             , Applicative
             , Functor
             , MonadError EvalError
             )
