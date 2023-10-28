{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Interpreter.Repl
  ( TestRepl (..)
  , TestReplState (..)
  ) where

import           Control.Monad.Catch
import           Control.Monad.State
import           SLang

newtype TestRepl a = TestRepl
  { unTestRepl :: StateT TestReplState IO a
  } deriving ( MonadFail
             , Monad , Functor , Applicative
             , MonadIO
             , MonadMask, MonadThrow, MonadCatch
             , MonadState TestReplState
             )

data TestReplState = TestReplState
  { outputs :: [String]
  , inputs  :: [String]
  } deriving (Eq, Show)

instance SLangRepl TestRepl where
  
  getInputLine _ = do
    console <- get
    let input = head $ inputs console
    put $ console { inputs = tail $ inputs console }
    return $ Just input

  outputStrLn s = do
    console <- get
    put $ console { outputs = s : outputs console }

instance Interative TestRepl where
  repl = runSLangRepl

instance Algorithm TestRepl where
  algorithmM = algorithmM_
  algorithmW = algorithmW_

instance Command TestRepl where
  interpret = interpret_
  parsing = parsing_
  typeinfer = typeinfer_