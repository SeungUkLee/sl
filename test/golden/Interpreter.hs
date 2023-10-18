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

instance SLangEval SLangGoldenTest where
  evaluate = evaluate_

instance SLangParser SLangGoldenTest where
  parse = parse_

instance SLangTypeInfer SLangGoldenTest where
  infer algorithm = algorithm
  algorithmM = algorithmM_
  algorithmW = algorithmW_
