{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}

module SLang
  ( -- * re-exports
    module SLang.Eval
  , module SLang.Parser
  , module SLang.Interative
  , module SLang.TypeInfer
  , module SLang.Pretty
  , module SLang.Program

  , main

  , SLangError (..)
  ) where

import           Control.Monad.Catch      (Exception (displayException),
                                           Handler (..), MonadCatch, MonadMask,
                                           MonadThrow,
                                           SomeException (SomeException),
                                           catches)
import qualified Data.Text                as T
import qualified Data.Text.IO             as TIO

import           Control.Monad.IO.Class   (MonadIO (..))
import           SLang.Error
import           SLang.Eval
import           SLang.Interative
import           SLang.Parser
import qualified SLang.Pretty             as SP
import           SLang.Pretty
import           SLang.Program
import           SLang.TypeInfer
import qualified System.Console.Haskeline as H
import           System.Exit              (ExitCode (ExitFailure, ExitSuccess))
import qualified System.IO                as IO

newtype SLangApp a = SLangApp
  { runSLangApp :: IO a
  } deriving ( Monad
             , Functor
             , Applicative
             , MonadIO
             , MonadMask
             , MonadCatch
             , MonadThrow
             )

-- | Command line entry points
main :: IO ()
main = catches (runSLangApp slang) handlers

slang :: (MonadIO m, Command m, Algorithm m, Interative m) => m ()
slang = do
  options <- liftIO optParse
  case options of
    ParseOpt i o               -> mkParsingCliPgm i o
    TypeOfOpt algorithm i o    -> mkTypeInferCliPgm algorithm i o
    InterpretOpt algorithm i o -> mkInterpretCliPgm algorithm i o
    REPLOpt                    -> repl

handlers :: [Handler IO ()]
handlers =
  [ Handler $ \case
     ParserError e -> printSLangException e
     TypeInferError e -> printSLangException e
     EvaluatorError e -> printSLangException e
  , Handler $ \case
      ExitSuccess -> return ()
      ExitFailure  _ -> return ()
  , Handler $ \(SomeException e) -> do
      printSLangException e
  ]
  where
    printSLangException e = SP.prettyprint IO.stderr $ T.pack $ displayException e

instance Interative SLangApp where
  repl = H.runInputT H.defaultSettings runSLangRepl

  cli cmd input output = do
    ih <- input
    oh <- output

    let (withIH, file) = unInputHandle ih
    let withOH = unOutputHandle oh

    actionWithIOHandle (action file) withIH withOH

    where
      action file i o = do
        code <- liftIO $ TIO.hGetContents i
        res <- cmd file code
        SP.prettyprint o res

  stdin = getStdinHandle
  inputFile = getInputFileHandle
  stdout = getStdoutHandle
  outputFile = getOutputFileHandle

instance Command SLangApp where
  interpret = interpret_
  typeinfer = typeinfer_
  parsing = parsing_

instance Algorithm SLangApp where
  algorithmM = algorithmM_
  algorithmW = algorithmW_
