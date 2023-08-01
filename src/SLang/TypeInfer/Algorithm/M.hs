{-# LANGUAGE FlexibleContexts #-}

module SLang.TypeInfer.Algorithm.M
  ( mAlgorithm
  , newTyVar
  ) where

import           Control.Monad.Except         (MonadError (throwError),
                                               replicateM)
import           Control.Monad.Reader         (MonadReader (ask, local))
import           Control.Monad.State          (MonadState (get, put))
import qualified Data.Set                     as Set

import           SLang.Eval.Syntax            (Bop (..), Const (..), Expr (..),
                                               LetBind (..))
import           SLang.TypeInfer.Error        (TypeError (UnificationError))
import           SLang.TypeInfer.State        (InferState (..))
import qualified SLang.TypeInfer.Substitution as Subst
import           SLang.TypeInfer.Substitution (Subst,
                                               Substitutable (apply, ftv), (@@))
import           SLang.TypeInfer.Type         (Scheme (..), TVar (..),
                                               Type (..))
import qualified SLang.TypeInfer.TypeEnv      as TypeEnv
import           SLang.TypeInfer.TypeEnv      (TypeEnv)

instantiation
  :: (MonadError TypeError m , MonadReader TypeEnv m , MonadState InferState m)
  => Scheme
  -> m Type
instantiation (Forall vars t) = do
  nvars <- mapM (const newTyVar) vars
  let s = Subst.fromList (zip vars nvars)
  return $ apply s t

generalization :: TypeEnv -> Type -> Scheme
generalization env t  = Forall as t
  where as = Set.toList $ ftv t `Set.difference` ftv env

unify
  :: (MonadError TypeError m)
  => Type
  -> Type
  -> m Subst
unify a b | a == b = return Subst.empty
unify (TVar name) t | not (checkOccurs name t) = return $ Subst.make name t
unify t (TVar name) | not (checkOccurs name t) = return $ Subst.make name t
unify (TFun t1 t2) (TFun t1' t2') = do
  s <- unify t1 t1'
  s' <- unify (apply s t2) (apply s t2')
  return $ s' @@ s
unify received expected = throwError $ UnificationError received expected

checkOccurs :: TVar -> Type -> Bool
checkOccurs x (TVar name)  = x == name
checkOccurs x (TFun t1 t2) = checkOccurs x t1 || checkOccurs x t2
checkOccurs _ TInt         = False
checkOccurs _ TBool        = False

newTyVar
  :: (MonadReader TypeEnv m, MonadState InferState m)
  => m Type
newTyVar = do
  s <- get
  put s{count = count s + 1}
  return $ TVar $ TV (letters !! count s)

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

mAlgorithm
  :: (MonadReader TypeEnv m, MonadState InferState m, MonadError TypeError m)
  => Expr
  -> Type
  -> m Subst
mAlgorithm expr expected = case expr of
  EConst (CInt _) -> unify TInt expected

  EConst (CBool _) -> unify TBool expected

  EVar name -> do
    typescheme <- TypeEnv.lookup name
    typ <- instantiation typescheme
    unify typ expected

  EAbs name e -> do
    tyenv <- ask
    a1 <- newTyVar
    a2 <- newTyVar
    s <- unify (TFun a1 a2) expected

    let tyenv' = TypeEnv.extend (apply s tyenv) (name, Forall [] $ apply s a1)
        a3 = apply s a2

    s' <- local (const tyenv') (mAlgorithm e a3)

    return $ s' @@ s

  EApp e1 e2 -> do
    tyenv <- ask
    a <- newTyVar
    s <- mAlgorithm e1 (TFun a expected)

    let tyenv' = apply s tyenv
        a2 = apply s a

    s' <- local (const tyenv') (mAlgorithm e2 a2)

    return $ s' @@ s

  ELet (LBVal name evalue) ebody -> do
    tyenv <- ask
    a <- newTyVar
    s <- mAlgorithm evalue a

    let tyenv' = apply s tyenv
        expected' = apply s expected
        a' = apply s a

    let typescheme = generalization tyenv' a'
        tyenv'' = TypeEnv.extend tyenv' (name, typescheme)
        expected'' = apply s expected'

    s' <- local (const tyenv'') (mAlgorithm ebody expected'')

    return $ s' @@ s

  ELet (LBRec fNname argName evalue) ebody -> do
    tyenv <- ask
    a <- newTyVar

    s <- local
      (const (TypeEnv.extend tyenv (fNname, Forall [] a)))
      (mAlgorithm (EAbs argName evalue) a)

    let tyenv' = apply s tyenv
        expected' = apply s expected
        a' = apply s a

    s' <- local
      (const (TypeEnv.extend tyenv' (fNname, generalization tyenv' a')))
      (mAlgorithm ebody expected')

    return $ s' @@ s

  EIf e1 e2 e3 -> do
    tyenv <- ask
    s <- mAlgorithm e1 TBool

    let tyenv' = apply s tyenv
        expected' = apply s expected

    s' <- local (const tyenv') (mAlgorithm e2 expected')

    let tyenv'' = apply s' tyenv'
        expected'' = apply s' expected'

    s'' <- local (const tyenv'') (mAlgorithm e3 expected'')

    return $ s'' @@ s' @@ s

  EOp bop e1 e2 -> case bop of
    Add -> binaryop TInt TInt
    Sub -> binaryop TInt TInt
    Mul -> binaryop TInt TInt
    Equal -> do
      a <- newTyVar
      binaryop a TBool
    where
      binaryop op r = do
        tyenv <- ask
        s <- unify r expected

        let tyenv' = apply s tyenv
            op' = apply s op

        s' <- local (const tyenv') (mAlgorithm e1 op')

        let tyenv'' = apply s' tyenv'
            op'' = apply s' op'

        s'' <- local (const tyenv'') (mAlgorithm e2 op'')

        return $ s'' @@ s' @@ s
