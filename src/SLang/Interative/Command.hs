{-# LANGUAGE FlexibleContexts #-}

module SLang.Interative.Command
  ( executeCmd
  , interpret
  , parsing
  , typeinfer
  , interpretWithFile
  ) where

import           Control.Monad.IO.Class        (MonadIO (liftIO))
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           SLang.Eval.Class              (SLangEval (..))
import           SLang.Eval.Domain             (Value)
import           SLang.Eval.Syntax             (Expr)
import           SLang.Interative.Cli.OptParse (TIAlgorithm (..))
import qualified SLang.Interative.Result       as Result
import           SLang.Interative.Result       (Result)
import           SLang.Parser.Class            (SLangParser (..))
import qualified SLang.Pretty                  as SP
import           SLang.TypeInfer.Class         (SLangTypeInfer (..))
import           SLang.TypeInfer.Type          (Type)
import           System.IO                     (Handle)

executeCmd :: (SP.Pretty a, MonadIO m) => (T.Text -> m a) -> Handle -> T.Text -> m ()
executeCmd cmd o txt = do
  res <- cmd txt
  SP.prettyprint o res

interpret :: (SLangParser m, SLangTypeInfer m, SLangEval m) => TIAlgorithm -> FilePath -> T.Text -> m Result
interpret algorithm file code = do
  (_, typ, val) <- interpret' file code
  return $ Result.Interpret typ val
  where
    interpret' = case algorithm of
      W -> interpretW_
      M -> interpretM_

parsing :: (SLangParser m) => FilePath -> T.Text -> m Result
parsing file code = do
  expr <- parsing_ file code

  return $ Result.Parse expr

typeinfer :: (SLangParser m, SLangTypeInfer m) => TIAlgorithm -> FilePath -> T.Text -> m Result
typeinfer algorithm file code = do
  (expr, typ) <- typeinfer' file code
  return $ Result.TypeInfer expr typ
  where
    typeinfer' = case algorithm of
      W -> typeinferW_
      M -> typeinferM_

interpretWithFile :: (SLangParser m, SLangTypeInfer m, SLangEval m, MonadIO m) => T.Text -> m Result
interpretWithFile file = do
  let striped = T.unpack $ T.strip file

  code <- liftIO $ TIO.readFile striped
  (_, typ, val) <- interpretM_ striped code

  return $ Result.Load typ val

parsing_ :: (SLangParser m) => FilePath -> T.Text -> m Expr
parsing_ file code = do
  parse file code

typeinferM_ :: (SLangParser m, SLangTypeInfer m) => FilePath -> T.Text -> m (Expr, Type)
typeinferM_ file code = do
  expr <- parsing_ file code
  typ <- inferM expr

  return (expr, typ)

typeinferW_ :: (SLangParser m, SLangTypeInfer m) => FilePath -> T.Text -> m (Expr, Type)
typeinferW_ file code = do
  expr <- parsing_ file code
  typ <- inferW expr

  return (expr, typ)

interpretM_ :: (SLangParser m, SLangTypeInfer m, SLangEval m) => FilePath -> T.Text -> m (Expr, Type, Value)
interpretM_ file code = do
  (expr, typ) <- typeinferM_ file code
  val <- evaluate expr

  return (expr, typ, val)

interpretW_ :: (SLangParser m, SLangTypeInfer m, SLangEval m) => FilePath -> T.Text -> m (Expr, Type, Value)
interpretW_ file code = do
  (expr, typ) <- typeinferW_ file code
  val <- evaluate expr

  return (expr, typ, val)
