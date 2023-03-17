module Eval (eval) where

import           Syntax

import qualified Data.Map as Map


type Env = Map.Map String Value

data Value
  = VInt Integer
  | VBool Bool
  | VClosure Closure

instance Show Value where
  show (VInt n)     = show n
  show (VBool b)    = show b
  show (VClosure _) = "<<function>>"

type Name = String
type Closure = (Name, Expr, Env)

getInt :: Value -> Maybe Integer
getInt (VInt n) = Just n
getInt _        = Nothing

binop :: Bop -> Value -> Value -> Maybe Value
binop op v1 v2 = case op of
  Add -> do
    a <- getInt v1
    b <- getInt v2
    return $ VInt (a + b)
  Sub -> do
    a <- getInt v1
    b <- getInt v2
    return $ VInt (a - b)
  Mul -> do
    a <- getInt v1
    b <- getInt v2
    return $ VInt (a * b)
  Equal -> case (v1, v2) of
    (VInt n1, VInt n2)   -> return $ VBool (n1 == n2)
    (VBool b1, VBool b2) -> return $ VBool (b1 == b2)
    _                    -> Nothing

eval' :: Env -> Expr -> Maybe Value
eval' env expr = case expr of
  Const (Int n)  ->  return $ VInt n
  Const (Bool b) ->  return $ VBool b
  Var name       ->  Map.lookup name env
  App func arg -> do
    VClosure (fname, fbody, fenv) <- eval' env func
    varg <- eval' env arg
    let nenv = Map.insert fname varg fenv
    eval' nenv fbody
  Abs name body -> return $ VClosure (name, body, env)
  Let name evalue body -> do
    v <- eval' env evalue
    let nenv = Map.insert name v env
    eval' nenv body
  If cond th el -> do
    VBool v <- eval' env cond
    eval' env (if v then th else el)
  Op bop e1 e2 -> do
    v1 <- eval' env e1
    v2 <- eval' env e2
    binop bop v1 v2

eval :: Expr -> Maybe Value
eval = eval' Map.empty
