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
import           Prettyprinter        (Doc)

import           SLang.Eval.Error     (EvalError (UnboundVariable))
import           SLang.Eval.Syntax    (Expr, Name)
import qualified SLang.Pretty         as SP
import           SLang.Pretty         (Pretty)

data Value
  = VInt Integer
  | VBool Bool
  | VClosure Closure
  deriving (Show, Eq)

type Closure = (FuncExpr, TermEnv)
data FuncExpr
  = RecFun (Name, Name, Expr)
  | Fun (Name, Expr)
  deriving (Show, Eq)

newtype TermEnv = TermEnv (Map.Map Name Value)
  deriving (Show, Eq)

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

instance Pretty Value where
  pretty = pprValue

pprValue :: Value -> Doc ann
pprValue (VInt n)      = SP.pretty n
pprValue (VBool True)  = SP.pretty "true"
pprValue (VBool False) = SP.pretty "false"
pprValue (VClosure _)  = SP.pretty "<<function>>"
