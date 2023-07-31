module SLang.TypeInfer.Substitution
  ( Subst
  , Substitutable (..)
  , empty
  , (@@)
  , make
  , fromList
  ) where

import qualified Data.Map                as Map
import qualified Data.Set                as Set

import           SLang.TypeInfer.Type    (Scheme (..), TVar, Type (..))
import qualified SLang.TypeInfer.TypeEnv as TypeEnv
import           SLang.TypeInfer.TypeEnv (TypeEnv)

newtype Subst = Subst (Map.Map TVar Type)
  deriving Show

empty :: Subst
empty = Subst Map.empty

make :: TVar -> Type -> Subst
make name t = Subst $ Map.singleton name t

fromList :: [(TVar, Type)] -> Subst
fromList li = Subst $ Map.fromList li

(@@) :: Subst -> Subst -> Subst
(@@) (Subst s1) (Subst s2) = Subst $ Map.map (apply (Subst s1)) s2 `Map.union` s1

class Substitutable a where
  apply :: Subst -> a -> a
  ftv :: a -> Set.Set TVar

instance Substitutable Type where
  apply (Subst s) (TVar n) =
    case Map.lookup n s of
      Nothing -> TVar n
      Just t  -> t
  apply s (TFun t1 t2) = TFun (apply s t1) (apply s t2)
  apply _ t            = t

  ftv (TVar n)     = Set.singleton n
  ftv TInt         = Set.empty
  ftv TBool        = Set.empty
  ftv (TFun t1 t2) = Set.union (ftv t1) (ftv t2)

instance Substitutable Scheme where
  apply (Subst s) (Forall vars t) = Forall vars $ apply (Subst $ foldr Map.delete s vars) t

  ftv (Forall vars t) = Set.difference (ftv t) (Set.fromList vars)

instance Substitutable TypeEnv where
  apply s = TypeEnv.map (apply s)

  ftv tenv = ftv $ TypeEnv.elems tenv

instance Substitutable a => Substitutable [a] where
  apply s = map $ apply s
  ftv = foldr (Set.union . ftv) Set.empty
