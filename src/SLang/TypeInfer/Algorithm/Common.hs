{-# LANGUAGE FlexibleContexts #-}

module SLang.TypeInfer.Algorithm.Common
  ( instantiation
  , newTyVar
  , unify
  , checkOccurs
  , generalization
  ) where

import           Control.Monad.Except         (MonadError (throwError),
                                               replicateM)
import           Control.Monad.Reader         (MonadReader (..))
import           Control.Monad.State          (MonadState (get, put))
import qualified Data.Set                     as Set
import qualified Data.Text                    as T

import           SLang.Eval.Syntax            (Expr)
import           SLang.TypeInfer.Error        (TypeError (..))
import           SLang.TypeInfer.State        (InferState (..))
import qualified SLang.TypeInfer.Substitution as Subst
import           SLang.TypeInfer.Substitution (Subst,
                                               Substitutable (apply, ftv), (@@))
import           SLang.TypeInfer.Type         (Scheme (..), TVar (..),
                                               Type (..))
import           SLang.TypeInfer.TypeEnv      (TypeEnv)

generalization :: TypeEnv -> Type -> Scheme
generalization env t  = Forall as t
  where as = Set.toList $ ftv t `Set.difference` ftv env

instantiation
  :: (MonadError TypeError m , MonadReader TypeEnv m , MonadState InferState m)
  => Scheme
  -> m Type
instantiation (Forall vars t) = do
  nvars <- mapM (const newTyVar) vars
  let s = Subst.fromList (zip vars nvars)
  return $ apply s t

newTyVar :: (MonadReader TypeEnv m, MonadState InferState m) => m Type
newTyVar = do
  s <- get
  put s{count = count s + 1}

  return $ TVar $ TV (letters !! count s)

  where
    letters :: [T.Text]
    letters = fmap T.pack $ [1..] >>= flip replicateM ['a'..'z']

unify :: (MonadError TypeError m) => Type -> Type -> Expr -> m Subst
unify a b _ | a == b = return Subst.empty
unify (TVar name) t _ | not (checkOccurs name t) = return $ Subst.make name t
unify t (TVar name) _ | not (checkOccurs name t) = return $ Subst.make name t
unify (TFun t1 t2) (TFun t1' t2') expr = do
  s <- unify t1 t1' expr
  s' <- unify (apply s t2) (apply s t2') expr
  return $ s' @@ s
unify received expected expr = throwError $ UnificationError received expected expr

checkOccurs :: TVar -> Type -> Bool
checkOccurs x (TVar name)  = x == name
checkOccurs x (TFun t1 t2) = checkOccurs x t1 || checkOccurs x t2
checkOccurs _ TInt         = False
checkOccurs _ TBool        = False
