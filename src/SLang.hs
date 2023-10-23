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
  , evaluate_
  , parse_
  , algorithmM_
  , algorithmW_
  , runMachine

  , SLangError (..)
  ) where

import           Control.Monad.Catch    (Exception (displayException),
                                         Handler (Handler), MonadCatch,
                                         MonadMask, MonadThrow (throwM),
                                         SomeException (SomeException), catches)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO

import           SLang.Eval
import           SLang.Interative
import           SLang.Parser
import qualified SLang.Pretty           as SP
import           SLang.Pretty
import           SLang.Program
import           SLang.TypeInfer        (InferState, SLangTypeInfer (..),
                                         Substitutable (apply), TVar (..),
                                         Type (..), TypeError, mAlgorithm,
                                         newTyVar, runSLangTIwithM,
                                         runSLangTIwithW, wAlgorithm)
import           System.Exit            (ExitCode (ExitFailure, ExitSuccess))
import qualified System.IO              as IO

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

data SLangError
  = ParserError ParseError
  | EvaluatorError EvalError
  | TypeInferError TypeError
  deriving Show

instance Exception SLangError where
  displayException (ParserError e)    = displayException e
  displayException (EvaluatorError e) = displayException e
  displayException (TypeInferError e) = displayException e

-- | Command line entry points
main :: IO ()
main = catches (runSLangApp slang) handlers

slang :: (FinalSLang m, MonadIO m) => m ()
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
  , Handler $ \(SomeException e) -> printSLangException e
  ]
  where
    printSLangException e = TIO.hPutStrLn IO.stderr $ T.pack $ displayException e

instance Interative SLangApp where
  repl = undefined
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
        liftIO $ SP.renderIO o $ SP.pretty res

  stdin = getStdinHandle
  inputFile = getInputFileHandle
  stdout = getStdoutHandle
  outputFile = getOutputFileHandle

instance Command SLangApp where
  interpret algorithm file code = do
    expr <- parse file code
    typ  <- infer algorithm expr
    val  <- evaluate expr

    return (val, typ)

  typeinfer algorithm file code = do
    expr <- parse file code
    typ  <- infer algorithm expr
    return (expr, typ)

  parsing = parse

instance Algorithm SLangApp where
instance SLangEval SLangApp where
  evaluate = evaluate_

instance SLangParser SLangApp where
  parse = parse_

instance SLangTypeInfer SLangApp where
  infer = id
  algorithmM = algorithmM_
  algorithmW = algorithmW_


evaluate_ :: (MonadThrow m) => Expr -> m Value
evaluate_ expr = runMachine runSLangEval expr EvaluatorError

parse_ :: (MonadThrow m) => FilePath -> T.Text -> m Expr
parse_ file txt = runMachine (runSLangParser file) txt ParserError

algorithmW_ :: (MonadThrow m) => Expr -> m Type
algorithmW_ expr = runMachine runSLangTIwithW expr TypeInferError

algorithmM_ :: (MonadThrow m) => Expr -> m Type
algorithmM_ expr = runMachine runSLangTIwithM expr TypeInferError

runMachine
  :: (MonadThrow m, Exception e)
  => (arg -> m (Either err res))
  -> arg
  -> (err -> e)
  -> m res
runMachine run arg slangErrConstructor = do
  result <- run arg
  case result of
    Left err  -> throwM $ slangErrConstructor err
    Right res -> return res
