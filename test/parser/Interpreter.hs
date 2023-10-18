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

instance SLangParser TestParser where
  parse file txt = do
    eitherResult <- runSLangParser file txt
    case eitherResult of
      Left err   -> throwError err
      Right expr -> return expr
