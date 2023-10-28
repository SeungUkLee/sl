{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Interpreter
  ( TestTypeInfer (..)
  ) where

import           Control.Monad.Except
import           SLang

newtype TestTypeInfer a = TestTypeInfer
  { runTestTypeInfer :: Except TypeError a
  } deriving ( Monad
             , Applicative
             , Functor
             , MonadError TypeError
             )
