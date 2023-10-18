{-# LANGUAGE FlexibleContexts #-}

module SLang.Interative.Command
  ( executeCmd
  , interpret
  , parsing
  , typeinfer
  , interpretWithFile
  ) where

import           Control.Monad.IO.Class  (MonadIO (liftIO))
import qualified Data.Text               as T
import qualified Data.Text.IO            as TIO
import           SLang.Eval.Class        (SLangEval (..))
import           SLang.Eval.Domain       (Value)
import           SLang.Eval.Syntax       (Expr)
import qualified SLang.Interative.Result as Result
import           SLang.Interative.Result (Result)
import           SLang.Parser.Class      (SLangParser (..))
import qualified SLang.Pretty            as SP
import           SLang.TypeInfer.Class   (SLangTypeInfer (..))
import           SLang.TypeInfer.Type    (Type)
import           System.IO               (Handle)

executeCmd :: (SP.Pretty a, MonadIO m) => (T.Text -> m a) -> Handle -> T.Text -> m ()
executeCmd cmd o txt = do
  res <- cmd txt
  SP.prettyprint o res

interpret :: (SLangParser m, SLangTypeInfer m, SLangEval m) => (Expr -> m Type) -> FilePath -> T.Text -> m Result
interpret algorithm file code = do
  (_, typ, val) <- interpret_ algorithm file code

  return $ Result.Interpret typ val

parsing :: (SLangParser m) => FilePath -> T.Text -> m Result
parsing file code = do
  expr <- parsing_ file code

  return $ Result.Parse expr

typeinfer :: (SLangParser m, SLangTypeInfer m) => (Expr -> m Type) -> FilePath -> T.Text -> m Result
typeinfer algorithm file code = do
  (expr, typ) <- typeinfer_ algorithm file code

  return $ Result.TypeInfer expr typ

interpretWithFile :: (SLangParser m, SLangTypeInfer m, SLangEval m, MonadIO m) => T.Text -> m Result
interpretWithFile file = do
  let striped = T.unpack $ T.strip file

  code <- liftIO $ TIO.readFile striped
  (_, typ, val) <- interpret_ algorithmM striped code

  return $ Result.Load typ val

parsing_ :: (SLangParser m) => FilePath -> T.Text -> m Expr
parsing_ file code = do
  parse file code

typeinfer_ :: (SLangParser m, SLangTypeInfer m) => (Expr -> m Type) -> FilePath -> T.Text -> m (Expr, Type)
typeinfer_ algo file code = do
  expr <- parsing_ file code
  typ <- infer algo expr

  return (expr, typ)

interpret_ :: (SLangParser m, SLangTypeInfer m, SLangEval m) => (Expr -> m Type) -> FilePath -> T.Text -> m (Expr, Type, Value)
interpret_ algo file code = do
  (expr, typ) <- typeinfer_ algo file code
  val <- evaluate expr

  return (expr, typ, val)
