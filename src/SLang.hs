{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}

module SLang
  ( -- * re-exports
    module SLang.Eval
  , module SLang.Parser
  , module SLang.Interative
  , module SLang.TypeInfer
  , module SLang.Pretty

  , main
  , evaluate_
  , inferM_
  , inferW_
  , parse_

  , SLangError (..)
  ) where

import           Control.Monad.Catch    (Exception (displayException),
                                         Handler (Handler), MonadCatch,
                                         MonadMask, MonadThrow (throwM),
                                         SomeException (SomeException), catches)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO

import qualified Data.Kind              as K
import           SLang.Eval             (EvalError, Expr, SLangEval (..), Value,
                                         runSLangEval)
import           SLang.Interative
import           SLang.Parser           (ParseError, SLangParser (..),
                                         runSLangParser)
import           SLang.Pretty
import           SLang.TypeInfer        (InferState, SLangTypeInfer (..), Type,
                                         TypeError, runSLangTIwithM,
                                         runSLangTIwithW)
import           System.Console.Repline (HaskelineT)
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

slang :: (Interative m, SLangParser m, SLangEval m, SLangTypeInfer m) => m ()
slang = do
  options <- liftIO optParse
  case options of
    InterpretOpt algorithm i o -> cli (interpret algorithm) i o
    ParseOpt i o               -> cli parsing i o
    TypeOfOpt algorithm i o    -> cli (typeinfer algorithm) i o
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
  cli = executeCli
  repl = loop

instance SLangEval SLangApp where
  evaluate = evaluate_

instance SLangParser SLangApp where
  parse = parse_

instance SLangTypeInfer SLangApp where
  inferM = inferM_
  inferW = inferW_

instance SLangEval (HaskelineT SLangApp) where
  evaluate = evaluate_

instance SLangParser (HaskelineT SLangApp) where
  parse = parse_

instance SLangTypeInfer (HaskelineT SLangApp) where
  inferM = inferM_
  inferW = inferW_

evaluate_ :: (MonadThrow m) => Expr -> m Value
evaluate_ expr = do
  eitherResult <- runSLangEval expr
  case eitherResult of
    Left err -> throwM $ EvaluatorError err
    Right v  -> return v

inferW_ :: (MonadThrow m) => Expr -> m Type
inferW_ = infer_ runSLangTIwithW

inferM_ :: (MonadThrow m) => Expr -> m Type
inferM_ = infer_ runSLangTIwithM

infer_
  :: (MonadThrow m)
  => ( forall (n :: K.Type -> K.Type)
     . Monad n
     => Expr
     -> n (Either TypeError (Type, InferState))
     )
  -> Expr
  -> m Type
infer_ ti expr = do
  eitherResult <- ti expr
  case eitherResult of
    Left err       -> throwM $ TypeInferError err
    Right (typ, _) -> return typ

parse_ :: (MonadThrow m) => FilePath -> T.Text -> m Expr
parse_ file txt = do
  eitherResult <- runSLangParser file txt
  case eitherResult of
    Left err   -> throwM $ ParserError err
    Right expr -> return expr
