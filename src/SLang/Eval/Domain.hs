{-# LANGUAGE FlexibleContexts #-}

module SLang.Eval.Domain
  ( Value (..)
  , TermEnv
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

type Closure = (Name, Expr, TermEnv)

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
  show (VInt n)     = show n
  show (VBool b)    = show b
  show (VClosure _) = "<<function>>"
