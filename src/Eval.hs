module Eval (runEval) where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Syntax

import qualified Data.Map             as Map

type Eval a = ReaderT Env (ExceptT String IO) a

type Env = Map.Map String Value

data Value
  = VInt Integer
  | VBool Bool
  | VClosure Closure

instance Show Value where
  show (VInt n)     = show n
  show (VBool b)    = show b
  show (VClosure _) = "<<function>>"

type Closure = (Name, Expr, Env)

eval :: Expr -> Eval Value
eval expr = case expr of
  Const (Int n)  ->  return $ VInt n
  Const (Bool b) ->  return $ VBool b
  Var name       ->  do
    env <- ask
    case Map.lookup name env of
      Nothing -> throwError ("unbound variable: " ++ name)
      Just value -> return value
  App func arg -> do
    VClosure (fname, fbody, fenv) <- eval func
    varg <- eval arg
    let nenv = Map.insert fname varg fenv
    local (const nenv) (eval fbody)
  Abs name body -> do
    env <- ask
    return $ VClosure (name, body, env)
  Let name evalue body -> do
    v <- eval evalue
    env <- ask
    let nenv = Map.insert name v env
    local (const nenv) (eval body)
  If cond th el -> do
    VBool v <- eval cond
    eval (if v then th else el)
  Op bop e1 e2 -> do
    v1 <- eval e1
    v2 <- eval e2
    binOp bop v1 v2

binOp :: Bop -> Value -> Value -> Eval Value
binOp op v1 v2 = case op of
  Add -> numOp (+) v1 v2
  Sub -> numOp (-) v1 v2
  Mul -> numOp (*) v1 v2
  Equal -> eqOp v1 v2

numOp :: (Integer -> Integer -> Integer) -> Value -> Value -> Eval Value
numOp op (VInt v1) (VInt v2) = return $ VInt $ op v1 v2 
numOp _ _ _ = throwError "error"

eqOp :: Value -> Value -> Eval Value
eqOp v1 v2 = case (v1, v2) of
  (VInt a, VInt b) -> return $ VBool $ a == b
  (VBool a, VBool b) -> return $ VBool $ a == b
  _ -> throwError "error"

runEval' :: Env -> Eval a -> IO (Either String a)
runEval' env ev = runExceptT $ runReaderT ev env

runEval :: Expr -> IO (Either String Value)
runEval e = runEval' Map.empty $ eval e
