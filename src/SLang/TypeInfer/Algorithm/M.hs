{-# LANGUAGE FlexibleContexts #-}

module SLang.TypeInfer.Algorithm.M
  ( mAlgorithm
  ) where

import           Control.Monad.Except             (MonadError)
import           Control.Monad.Reader             (MonadReader (ask, local))
import           Control.Monad.State              (MonadState)

import           SLang.Eval.Syntax                (Bop (..), Const (..),
                                                   Expr (..), LetBind (..))
import           SLang.TypeInfer.Algorithm.Common (generalization,
                                                   instantiation, newTyVar,
                                                   unify)
import           SLang.TypeInfer.Error            (TypeError (..))
import           SLang.TypeInfer.State            (InferState (..))
import           SLang.TypeInfer.Substitution     (Subst, Substitutable (..),
                                                   (@@))
import           SLang.TypeInfer.Type             (Scheme (..), Type (..))
import qualified SLang.TypeInfer.TypeEnv          as TypeEnv
import           SLang.TypeInfer.TypeEnv          (TypeEnv)

mAlgorithm
  :: (MonadReader TypeEnv m, MonadState InferState m, MonadError TypeError m)
  => Expr
  -> Type
  -> m Subst
mAlgorithm expr expected = case expr of
  EConst (CInt _) -> unify TInt expected expr

  EConst (CBool _) -> unify TBool expected expr

  EVar name -> do
    typescheme <- TypeEnv.lookup name
    typ <- instantiation typescheme
    unify typ expected expr

  EAbs name e -> do
    tyenv <- ask
    a1 <- newTyVar
    a2 <- newTyVar
    s <- unify (TFun a1 a2) expected expr

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

    s' <- local (const tyenv'') (mAlgorithm ebody expected')

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
        s <- unify r expected expr

        let tyenv' = apply s tyenv
            op' = apply s op

        s' <- local (const tyenv') (mAlgorithm e1 op')

        let tyenv'' = apply s' tyenv'
            op'' = apply s' op'

        s'' <- local (const tyenv'') (mAlgorithm e2 op'')

        return $ s'' @@ s' @@ s
