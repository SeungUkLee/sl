{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module SLang.TypeInfer
  ( inferExpr
  ) where

import           Control.Monad.Except         (ExceptT, MonadError, runExceptT)
import           Control.Monad.Reader         (MonadReader, ReaderT (..))
import           Control.Monad.State          (MonadState, State, runState)
import           SLang.Eval.Syntax            (Expr)


import           SLang.TypeInfer.Algorithm.M  (mAlgorithm, newTyVar)
import           SLang.TypeInfer.Error        (TypeError)
import qualified SLang.TypeInfer.State        as InferState
import           SLang.TypeInfer.State        (InferState)
import           SLang.TypeInfer.Substitution (Substitutable (apply))
import           SLang.TypeInfer.Type         (Type)
import qualified SLang.TypeInfer.TypeEnv      as TypeEnv
import           SLang.TypeInfer.TypeEnv      (TypeEnv)

newtype TypeInfer a = TypeInfer
  { runTypeInfer :: ReaderT TypeEnv (ExceptT TypeError (State InferState)) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadError TypeError
             , MonadState InferState
             , MonadReader TypeEnv
             )

runTypeInfer' :: TypeInfer a -> (Either TypeError a, InferState)
runTypeInfer' t = runState (runExceptT $ runReaderT (runTypeInfer t) TypeEnv.empty) InferState.empty

inferExpr :: Expr -> Either TypeError Type
inferExpr expr = fst $ runTypeInfer' infer
  where
    infer = do
      a <- newTyVar
      subst <- mAlgorithm expr a
      return $ apply subst a
