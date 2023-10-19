{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module SLang.Eval
  ( -- * re-exports
    module SLang.Eval.Domain
  , module SLang.Eval.Error
  , module SLang.Eval.Syntax
  , module SLang.Eval.Class

  , runSLangEval

  , SLangEval (..)
  ) where

import           Control.Monad.Except (MonadError (throwError), runExceptT)
import           Control.Monad.Reader (MonadReader (ask, local),
                                       ReaderT (runReaderT))

import           SLang.Eval.Class     (SLangEval (..))
import qualified SLang.Eval.Domain    as TermEnv
import           SLang.Eval.Domain    (Closure, FuncExpr (..), TermEnv,
                                       Value (..), empty)
import           SLang.Eval.Error     (EvalError (..))
import           SLang.Eval.Syntax    (Bop (..), Const (..), Expr (..),
                                       LetBind (..))

runSLangEval :: Monad m => Expr -> m (Either EvalError Value)
runSLangEval expr = runExceptT $ runReaderT (eval expr) empty

eval :: (MonadReader TermEnv m, MonadError EvalError m) => Expr -> m Value
eval (EConst (CInt n)) = return $ VInt n
eval (EConst (CBool b)) = return $ VBool b
eval (EVar name) = TermEnv.lookup name

eval (EApp func arg) = do
  v <- eval func
  (funcExpr, fenv) <- getClosure v
  varg <- eval arg
  case funcExpr of
    Fun (fname, fbody) -> do
      let nenv = TermEnv.extend fenv (fname, varg)
      local (const nenv) (eval fbody)

    RecFun (fname, argName, fbody) -> do
      let nenv = TermEnv.extend fenv (argName, varg)
      let nenv' = TermEnv.extend nenv (fname, VClosure (funcExpr, fenv))
      local (const nenv') (eval fbody)

eval (EAbs name body) = do
  env <- ask
  return $ VClosure (Fun (name, body), env)

eval (ELet (LBVal name evalue) body) = do
  v <- eval evalue
  env <- ask
  let nenv = TermEnv.extend env (name, v)
  local (const nenv) (eval body)

eval (ELet (LBRec fNname argName evalue) body) = do
  env <- ask
  let closure = VClosure (RecFun (fNname, argName, evalue), env)
  let nenv = TermEnv.extend env (fNname, closure)
  local (const nenv) (eval body)

eval (EIf cond th el) = do
  v <- eval cond
  bool <- getBool v
  eval (if bool then th else el)

eval (EOp bop e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  binOp bop v1 v2

binOp :: (MonadError EvalError m) => Bop -> Value -> Value -> m Value
binOp Add v1 v2   = numOp (+) v1 v2
binOp Sub v1 v2   = numOp (-) v1 v2
binOp Mul v1 v2   = numOp (*) v1 v2
binOp Equal v1 v2 = eqOp v1 v2

numOp :: (MonadError EvalError m) => (Integer -> Integer -> Integer) -> Value -> Value -> m Value
numOp op (VInt v1) (VInt v2) = return $ VInt $ op v1 v2
numOp _ _ _                  = throwError $ TypeMissmatch "arithmetic operations expected number"

eqOp :: (MonadError EvalError m) => Value -> Value -> m Value
eqOp (VInt a) (VInt b)   = return $ VBool $ a == b
eqOp (VBool a) (VBool b) = return $ VBool $ a == b
eqOp _ _                 = throwError $ TypeMissmatch "equal operaions expected number or boolean"

getClosure :: (MonadError EvalError m) => Value -> m Closure
getClosure (VClosure c) = return c
getClosure _            = throwError $ TypeMissmatch "this is not a function"

getBool :: (MonadError EvalError m) => Value -> m Bool
getBool (VBool b) = return b
getBool _         = throwError $ TypeMissmatch "this is not a bool"
