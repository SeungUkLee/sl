{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Interpreter
  ( SLangGoldenTest (..)
  ) where

import           Control.Monad.Catch    (MonadThrow)
import           Control.Monad.IO.Class (MonadIO)
import           SLang                  

newtype SLangGoldenTest a = SLangTest
  { runSLangGoldenTest :: IO a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadThrow
             )

instance Command SLangGoldenTest where
  interpret = interpret_
  parsing = parsing_
  typeinfer = typeinfer_

instance Algorithm SLangGoldenTest where
  algorithmM = algorithmM_
  algorithmW = algorithmW_