{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module SLang.Eval
  ( evalExpr
  ) where

import           Control.Monad.Except (Except, MonadError (throwError),
                                       runExcept)
import           Control.Monad.Reader (MonadReader (ask, local), ReaderT (..))

import qualified SLang.Eval.Domain    as TermEnv
import           SLang.Eval.Domain    (FuncExpr (..), TermEnv, Value (..), Closure)
import           SLang.Eval.Error     (EvalError (..))
import           SLang.Eval.Syntax    (Bop (..), Const (..), Expr (..),
                                       LetBind (..))

newtype Eval a = Eval
  { runEval :: ReaderT TermEnv (Except EvalError) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadError EvalError
             , MonadReader TermEnv
             )

eval :: Expr -> Eval Value
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

binOp :: Bop -> Value -> Value -> Eval Value
binOp Add v1 v2   = numOp (+) v1 v2
binOp Sub v1 v2   = numOp (-) v1 v2
binOp Mul v1 v2   = numOp (*) v1 v2
binOp Equal v1 v2 = eqOp v1 v2

numOp :: (Integer -> Integer -> Integer) -> Value -> Value -> Eval Value
numOp op (VInt v1) (VInt v2) = return $ VInt $ op v1 v2
numOp _ _ _                  = throwError $ TypeMissmatch "arithmetic operations expected number"

eqOp :: Value -> Value -> Eval Value
eqOp (VInt a) (VInt b)   = return $ VBool $ a == b
eqOp (VBool a) (VBool b) = return $ VBool $ a == b
eqOp _ _                 = throwError $ TypeMissmatch "equal operaions expected number or boolean"

getClosure :: Value -> Eval Closure
getClosure (VClosure c) = return c
getClosure _ = throwError $ TypeMissmatch "this is not a function"

getBool :: Value -> Eval Bool
getBool (VBool b) = return b
getBool _ = throwError $ TypeMissmatch "this is not a bool"

evalExpr :: Expr -> Either EvalError Value
evalExpr e = runExcept $ runReaderT (runEval $ eval e) TermEnv.empty
