{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Interpreter
  ( TestCli (..)
  ) where

import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import qualified SLang.Pretty           as SP

import           Control.Monad.Catch    (MonadCatch, MonadMask, MonadThrow,
                                         bracket)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           GHC.IO.Handle.Types    (Handle (FileHandle))
import           SLang
import qualified System.IO              as IO hiding (hClose, openFile)

newtype TestCli a = TestCli
  { runCli :: IO a
  } deriving ( MonadIO
             , Monad
             , Applicative
             , Functor
             , MonadMask
             , MonadCatch
             , MonadThrow
             )

instance SLangCli TestCli where
  cli cmd input output = do
    ih <- input
    oh <- output

    let (actWithIH, file) = unInputHandle ih
    let actWithOH = unOutputHandle oh

    actionWithIOHandle (action file) actWithIH actWithOH

    where
      action file i o = do
        let isStdin = case i of
              FileHandle fp _ -> fp == "<stdin>"
              _               -> False
        code <- if isStdin then return "let x = 1 in x" else liftIO $ TIO.hGetContents i
        res <- cmd file code

        let isStdout = case o of
              FileHandle fp _ -> fp == "<stdout>"
              _               -> False
        if isStdout then return () else liftIO $ SP.renderIO o $ SP.pretty res

  stdin = getStdinHandle
  inputFile = getInputFileHandle
  stdout = getStdoutHandle
  outputFile file = do
    return $ OutputHandle (bracket (openFile (T.unpack file) IO.WriteMode) hClose)


instance SLangEval TestCli where
  evaluate = evaluate_

instance SLangTypeInfer TestCli where
  infer algorithm = algorithm
  algorithmM = algorithmM_
  algorithmW = algorithmW_

instance SLangParser TestCli where
  parse = parse_
