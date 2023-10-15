{-# LANGUAGE FlexibleContexts #-}

module SLang.TypeInfer.Algorithm.W
  ( wAlgorithm
  ) where

import           Control.Monad.Except             (MonadError)
import           Control.Monad.Reader             (MonadReader (ask, local))
import           Control.Monad.State              (MonadState)

import           SLang.Eval.Syntax                (Bop (..), Const (..),
                                                   Expr (..), LetBind (..))
import           SLang.TypeInfer.Algorithm.Common (generalization,
                                                   instantiation, newTyVar,
                                                   unify)
import           SLang.TypeInfer.Error            (TypeError)
import           SLang.TypeInfer.State            (InferState)
import qualified SLang.TypeInfer.Substitution     as Subst
import           SLang.TypeInfer.Substitution     (Subst, apply, (@@))
import           SLang.TypeInfer.Type             (Scheme (Forall), Type (..))
import qualified SLang.TypeInfer.TypeEnv          as TypeEnv
import           SLang.TypeInfer.TypeEnv          (TypeEnv)


wAlgorithm
  :: (MonadReader TypeEnv m, MonadState InferState m, MonadError TypeError m)
  => Expr
  -> m (Subst, Type)
wAlgorithm expr = case expr of
  EConst (CInt _) -> return (Subst.empty, TInt)

  EConst (CBool _) -> return (Subst.empty, TBool)

  EVar name -> do
    typescheme <- TypeEnv.lookup name
    typ <- instantiation typescheme

    return (Subst.empty, typ)

  EAbs name e -> do
    tyenv <- ask
    a <- newTyVar

    let tyenv' = TypeEnv.extend tyenv (name, Forall [] a)
    (s1, t1) <- local (const tyenv') (wAlgorithm e)

    return (s1, TFun (apply s1 a) t1)

  EApp e1 e2 -> do
    env <- ask
    tv <- newTyVar

    (s1, t1) <- local (const env) $ wAlgorithm e1
    (s2, t2) <- local (const $ apply s1 env) $ wAlgorithm e2

    s3 <- unify (apply s2 t1) (TFun t2 tv) e1

    return (s3 @@ s2 @@ s1, apply s3 tv)

  ELet (LBVal name evalue) ebody -> do

{- | W (Γ, let x = e1 in e2)
let (S1, t1) = W(Γ, E1)
    (S2, t2) = W(S1Γ + x : GEN S1Γ(t1), E2)
in (S2S1, τ2)
-}
    tyenv <- ask
    (s1, t1) <- local (const tyenv) (wAlgorithm evalue)

    let tyenv' = apply s1 tyenv -- ^ S1Γ
    let typescheme = generalization tyenv' t1 -- ^ GEN S1Γ(t1)
    let tyenv'' = TypeEnv.extend tyenv' (name, typescheme) -- ^ S1Γ + x : GEN S1Γ(t1)

    (s2, t2) <- local (const tyenv'') (wAlgorithm ebody)

    return (s2 @@ s1, t2)

  ELet (LBRec fNname argName evalue) ebody -> do
    tyenv <- ask
    a <- newTyVar

    (s1, t1) <- local
      (const (TypeEnv.extend tyenv (fNname, Forall [] a)))
      (wAlgorithm (EAbs argName evalue))


    let tyenv' = apply s1 tyenv
    let typescheme = generalization tyenv' (apply s1 t1)

    (s2, t2) <- local (const (TypeEnv.extend tyenv' (fNname, typescheme))) (wAlgorithm ebody)

    return (s2 @@ s1, t2)

  EIf e1 e2 e3 -> do
    tyenv <- ask

    (s1, t1) <- wAlgorithm e1
    s2 <- unify t1 TBool e1

    let tyenv' = apply s2 $ apply s1 tyenv
    (s3, t2) <- local (const tyenv') (wAlgorithm e2)

    let tyenv'' = apply s3 tyenv'
    (s4, t3) <- local (const tyenv'') (wAlgorithm e3)

    s5 <- unify t3 t2 e3

    return (s5 @@ s4 @@ s3 @@ s2 @@ s1, t3)

  EOp bop e1 e2 -> case bop of
    Add -> binaryop TInt TInt
    Sub -> binaryop TInt TInt
    Mul -> binaryop TInt TInt
    Equal -> do
      a <- newTyVar
      binaryop TBool a
    where
      binaryop op r = do
        tyenv <- ask
        (s1, t1) <- local (const tyenv) (wAlgorithm e1)
        s2 <- unify t1 r e1

        (s3, t') <- local (const $ apply s2 $ apply s1 tyenv) (wAlgorithm e2)
        s4 <- unify t' t1 e2

        return (s4 @@ s3 @@ s2 @@ s1, op)
