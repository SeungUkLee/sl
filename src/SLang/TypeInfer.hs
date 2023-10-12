{-# LANGUAGE FlexibleContexts #-}

module SLang.TypeInfer
  ( -- * re-exports
    module SLang.TypeInfer.Algorithm
  , module SLang.TypeInfer.Error
  , module SLang.TypeInfer.State
  , module SLang.TypeInfer.Substitution
  , module SLang.TypeInfer.Type
  , module SLang.TypeInfer.TypeEnv

  , runSLangTypeInfer

  , SLangTypeInfer (..)
  ) where

import           Control.Monad.Except         (MonadError, runExceptT)
import           Control.Monad.Reader         (MonadReader,
                                               ReaderT (runReaderT))
import           Control.Monad.State          (MonadState, StateT (runStateT))
import           SLang.Eval                   (Expr)


import           SLang.TypeInfer.Algorithm    (mAlgorithm, newTyVar)
import           SLang.TypeInfer.Class        (SLangTypeInfer (..))
import           SLang.TypeInfer.Error        (TypeError)
import qualified SLang.TypeInfer.State        as SState
import           SLang.TypeInfer.State        (InferState)
import           SLang.TypeInfer.Substitution (Substitutable (apply))
import           SLang.TypeInfer.Type         (Type)
import           SLang.TypeInfer.TypeEnv      (TypeEnv, empty)

runSLangTypeInfer :: (Monad m) => Expr -> m (Either TypeError (Type, InferState))
runSLangTypeInfer expr = runExceptT $ runStateT (runReaderT (inferExpr expr) empty) SState.empty

inferExpr :: (MonadState InferState m, MonadError TypeError m, MonadReader TypeEnv m) => Expr -> m Type
inferExpr expr = do
  a <- newTyVar
  subst <- mAlgorithm expr a
  return $ apply subst a
