{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE RankNTypes      #-}

module SLang.Program
  ( Command (..)
  , Algorithm (..)
  , Interative (..)

  , mkInterpretPgm
  , mkTypeInferPgm
  , mkInterpretCliPgm
  , mkTypeInferCliPgm
  , mkParsingCliPgm
  , mkCliPgm

  , interpret_
  , typeinfer_
  , evaluate_
  , parsing_
  , algorithmM_
  , algorithmW_
  )
where

import           Control.Monad.Catch      (Exception, MonadThrow (throwM))
import           Control.Monad.Except     (MonadError, runExceptT)
import qualified Data.Kind                as K
import qualified Data.Text                as T
import qualified System.Console.Haskeline as H

import           SLang.Error
import           SLang.Eval
import           SLang.Interative.Cli
import           SLang.Parser
import           SLang.Pretty
import           SLang.TypeInfer

class Interative m where
  cli :: (Pretty a) => (FilePath -> T.Text -> m a) -> m (InputHandle m) -> m (OutputHandle m) -> m ()
  repl :: m ()

  inputFile :: T.Text -> m (InputHandle m)
  outputFile :: T.Text -> m (OutputHandle m)

  stdin :: m (InputHandle m)
  stdout :: m (OutputHandle m)

class Command m where
  interpret :: (Expr -> m Type) -> (FilePath -> T.Text -> m (Value, Type))
  typeinfer :: (Expr -> m Type) -> (FilePath -> T.Text -> m (Expr, Type))
  parsing :: FilePath -> T.Text -> m T.Text

class Algorithm m where
  algorithmW, algorithmM :: Expr -> m Type

instance (MonadThrow m) => Command (H.InputT m) where
  interpret = interpret_
  parsing = parsing_
  typeinfer = typeinfer_

instance (MonadThrow m) => Algorithm (H.InputT m) where
  algorithmW = algorithmW_
  algorithmM = algorithmM_

interpret_ :: (MonadThrow m) => (Expr -> m Type) -> FilePath -> T.Text -> m (Value, Type)
interpret_ algorithm file code = do
  (expr, typ) <- typeinfer_ algorithm file code
  val  <- evaluate_ expr

  return (val, typ)

typeinfer_ :: (MonadThrow m) => (Expr -> m Type) -> FilePath -> T.Text -> m (Expr, Type)
typeinfer_ algorithm file code = do
  expr <- parse_ file code
  typ  <- algorithm expr

  return (expr, typ)

parsing_ :: (MonadThrow m) => FilePath -> T.Text -> m T.Text
parsing_ file code = do
  expr <- parse_ file code
  return $ T.pack $ show expr ++ "\n"

evaluate_ :: (MonadThrow m) => Expr -> m Value
evaluate_ expr = run evaluate expr EvaluatorError

parse_ :: (MonadThrow m) => FilePath -> T.Text -> m Expr
parse_ file txt = run (parse file) txt ParserError

algorithmW_ :: (MonadThrow m) => Expr -> m Type
algorithmW_ expr = run typeinferW expr TypeInferError

algorithmM_ :: (MonadThrow m) => Expr -> m Type
algorithmM_ expr = run typeinferM expr TypeInferError

run
  :: (MonadThrow m, Exception e)
  => (forall (n :: K.Type -> K.Type) . (MonadError err n) => arg -> n res)
  -> arg
  -> (err -> e)
  -> m res
run target arg errConstructor = do
  result <- runExceptT $ target arg
  case result of
    Left err  -> throwM $ errConstructor err
    Right res -> return res

{- | make SLang project program -}

mkInterpretPgm :: (Command m, Algorithm m) => TIAlgorithm -> FilePath -> T.Text -> m (Value, Type)
mkInterpretPgm algorithm = case algorithm of
  W -> interpret algorithmW
  M -> interpret algorithmM

mkTypeInferPgm :: (Command m, Algorithm m) => TIAlgorithm -> FilePath -> T.Text -> m (Expr, Type)
mkTypeInferPgm algorithm = case algorithm of
  W -> typeinfer algorithmW
  M -> typeinfer algorithmM

mkInterpretCliPgm :: (Command m, Interative m, Algorithm m) => TIAlgorithm -> Input -> Output -> m ()
mkInterpretCliPgm algorithm = mkCliPgm (mkInterpretPgm algorithm)

mkTypeInferCliPgm :: (Command m, Interative m, Algorithm m) => TIAlgorithm -> Input -> Output -> m ()
mkTypeInferCliPgm algorithm = mkCliPgm (mkTypeInferPgm algorithm)

mkParsingCliPgm :: (Command m, Interative m) => Input -> Output -> m ()
mkParsingCliPgm = mkCliPgm parsing

mkCliPgm :: (Interative m, Pretty a) => (FilePath -> T.Text -> m a) -> Input -> Output -> m ()
mkCliPgm cmd input output = case (input, output) of
  (Stdin, Stdout) -> cli cmd stdin stdout
  (Stdin, OutputFile file) -> cli cmd stdin (outputFile $ T.pack file)
  (InputFile file, Stdout) -> cli cmd (inputFile $ T.pack file) stdout
  (InputFile file, OutputFile file') -> cli cmd (inputFile $ T.pack file) (outputFile $ T.pack file')
