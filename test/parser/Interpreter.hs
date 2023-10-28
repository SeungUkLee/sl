{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Interpreter 
  (TestParser (..)
  ) where

import           Control.Monad.Except
import           SLang

newtype TestParser a = TestParser
  { runTestParser :: Except ParseError a
  } deriving ( Monad
             , Applicative
             , Functor
             , MonadError ParseError
             )
