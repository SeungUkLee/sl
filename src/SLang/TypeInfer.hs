{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE RankNTypes       #-}

module SLang.TypeInfer
  ( -- * re-exports
    module SLang.TypeInfer.Algorithm
  , module SLang.TypeInfer.Error
  , module SLang.TypeInfer.State
  , module SLang.TypeInfer.Substitution
  , module SLang.TypeInfer.Type
  , module SLang.TypeInfer.TypeEnv

  , runSLangTIwithM
  , runSLangTIwithW

  , SLangTypeInfer (..)
  ) where

import           Control.Monad.Except         (MonadError, runExceptT)
import           Control.Monad.Reader         (MonadReader,
                                               ReaderT (runReaderT))
import           Control.Monad.State          (MonadState, StateT (runStateT))
import           SLang.Eval                   (Expr)


import qualified Data.Kind                    as K
import           SLang.TypeInfer.Algorithm    (mAlgorithm, newTyVar, wAlgorithm)
import           SLang.TypeInfer.Class        (SLangTypeInfer (..))
import           SLang.TypeInfer.Error        (TypeError)
import qualified SLang.TypeInfer.State        as SState
import           SLang.TypeInfer.State        (InferState)
import           SLang.TypeInfer.Substitution (Substitutable (apply))
import           SLang.TypeInfer.Type         (Type)
import           SLang.TypeInfer.TypeEnv      (TypeEnv, empty)

runSLangTIwithM :: (Monad m) => Expr -> m (Either TypeError (Type, InferState))
runSLangTIwithM = runSLangTI inferExprM

runSLangTIwithW :: (Monad m) => Expr -> m (Either TypeError (Type, InferState))
runSLangTIwithW = runSLangTI inferExprW

inferExprM :: (MonadState InferState m, MonadError TypeError m, MonadReader TypeEnv m) => Expr -> m Type
inferExprM expr = do
  a <- newTyVar
  subst <- mAlgorithm expr a
  return $ apply subst a

inferExprW :: (MonadState InferState m, MonadError TypeError m, MonadReader TypeEnv m) => Expr -> m Type
inferExprW expr = do
  (s, t) <- wAlgorithm expr
  return $ apply s t

runSLangTI
  :: Monad m
  => ( forall (n :: K.Type -> K.Type)
     . (MonadReader TypeEnv n, MonadError TypeError n, MonadState InferState n)
     => Expr
     -> n Type
     )
  -> Expr
  -> m (Either TypeError (Type, InferState))
runSLangTI inf expr = runExceptT $ runStateT (runReaderT (inf expr) empty) SState.empty
