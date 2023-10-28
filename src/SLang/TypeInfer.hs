{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE RankNTypes       #-}

module SLang.TypeInfer
  ( -- * re-exports
    module SLang.TypeInfer.Algorithm
  , module SLang.TypeInfer.Error
  , module SLang.TypeInfer.Type

  , typeinferW
  , typeinferM
  ) where

import           Control.Monad.Except         (MonadError)
import           Control.Monad.Reader         (MonadReader,
                                               ReaderT (runReaderT))
import           Control.Monad.State          (MonadState, StateT (runStateT))
import           SLang.Eval                   (Expr)

import qualified Data.Kind                    as K
import           SLang.TypeInfer.Algorithm    (mAlgorithm, newTyVar, wAlgorithm)
import           SLang.TypeInfer.Error        (TypeError)
import qualified SLang.TypeInfer.State        as SState
import           SLang.TypeInfer.State        (InferState)
import           SLang.TypeInfer.Substitution (Substitutable (apply))
import           SLang.TypeInfer.Type         (TVar (..), Type (..))
import           SLang.TypeInfer.TypeEnv      (TypeEnv, empty)

typeinferW :: (MonadError TypeError m) => Expr -> m Type
typeinferW = runTI $ \expr -> do
  (s, t) <- wAlgorithm expr
  return $ apply s t

typeinferM :: (MonadError TypeError m) => Expr -> m Type
typeinferM = runTI $ \expr -> do
  a <- newTyVar
  subst <- mAlgorithm expr a
  return $ apply subst a

runTI
  :: (MonadError TypeError m)
  => ( forall (n :: K.Type -> K.Type)
     . (MonadReader TypeEnv n, MonadState InferState n, MonadError TypeError n)
     => Expr
     -> n Type
     )
  -> Expr
  -> m Type
runTI ti expr = do
  (typ, _) <- runStateT (runReaderT (ti expr) empty) SState.empty
  return typ
