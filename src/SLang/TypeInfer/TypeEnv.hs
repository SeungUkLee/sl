{-# LANGUAGE FlexibleContexts #-}

module SLang.TypeInfer.TypeEnv
  ( TypeEnv
  , lookup
  , extend
  , empty
  , map
  , elems
  )
where

import           Control.Monad.Except  (MonadError, throwError)
import           Control.Monad.Reader  (MonadReader, ask)
import qualified Data.Map              as Map
import           Prelude               hiding (lookup, map)

import           SLang.Eval.Syntax     (Name)
import           SLang.TypeInfer.Error (TypeError (UnboundVar))
import           SLang.TypeInfer.Type  (Scheme)

newtype TypeEnv = TypeEnv (Map.Map Name Scheme)

lookup
  :: (MonadReader TypeEnv m, MonadError TypeError m)
  => Name
  -> m Scheme
lookup name = do
  (TypeEnv env) <- ask
  case Map.lookup name env of
    Nothing -> throwError $ UnboundVar name
    Just s  -> return s

extend :: TypeEnv -> (Name, Scheme) -> TypeEnv
extend (TypeEnv env) (x, s) = TypeEnv $ Map.insert x s env

empty :: TypeEnv
empty = TypeEnv Map.empty

map :: (Scheme -> Scheme) -> TypeEnv -> TypeEnv
map f (TypeEnv tenv) = TypeEnv $ Map.map f tenv

elems :: TypeEnv -> [Scheme]
elems (TypeEnv tenv) = Map.elems tenv
