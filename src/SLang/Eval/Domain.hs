{-# LANGUAGE FlexibleContexts #-}

module SLang.Eval.Domain
  ( Value (..)
  , FuncExpr (..)
  , TermEnv
  , Closure
  , lookup
  , extend
  , empty
  ) where

import           Control.Monad.Except (MonadError (throwError))
import           Control.Monad.Reader (MonadReader (ask))
import qualified Data.Map             as Map
import           Prelude              hiding (lookup)

import           SLang.Eval.Error     (EvalError (UnboundVariable))
import           SLang.Eval.Syntax    (Expr, Name)

data Value
  = VInt Integer
  | VBool Bool
  | VClosure Closure

type Closure = (FuncExpr, TermEnv)
data FuncExpr
  = RecFun (Name, Name, Expr)
  | Fun (Name, Expr)

newtype TermEnv = TermEnv (Map.Map Name Value)
  deriving Show

lookup
  :: (MonadReader TermEnv m, MonadError EvalError m)
  => Name
  -> m Value
lookup name = do
  (TermEnv env) <- ask
  case Map.lookup name env of
    Nothing -> throwError $ UnboundVariable name
    Just s  -> return s

extend
  :: TermEnv
  -> (Name, Value)
  -> TermEnv
extend (TermEnv env) (x, s) = TermEnv $ Map.insert x s env

empty :: TermEnv
empty = TermEnv Map.empty

instance Show Value where
  show (VInt n)      = show n
  show (VBool True)  = "true"
  show (VBool False) = "false"
  show (VClosure _)  = "<<function>>"
