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

instance SLangTypeInfer TestTypeInfer where
  infer = id

  algorithmM expr = do
    eitherResult <- runSLangTIwithM expr
    case eitherResult of
      Left err  -> throwError err
      Right typ -> return typ

  algorithmW expr = do
    eitherResult <- runSLangTIwithW expr
    case eitherResult of
      Left err  -> throwError err
      Right typ -> return typ
