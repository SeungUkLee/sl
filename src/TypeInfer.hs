{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module TypeInfer where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Syntax

import           Control.Monad.State
import qualified Data.Map             as Map
import qualified Data.Set             as Set

data Type
  = TInt
  | TBool
  | TVar TVar
  | TFun Type Type
  deriving ( Eq
           , Show
           , Ord
           )

newtype TVar = TV String
  deriving ( Eq
           , Show
           , Ord
           )

data Scheme = Forall [TVar] Type
  deriving ( Eq
           , Show
           , Ord
           )

newtype TypeInfer a = TypeInfer
  { runTI :: ReaderT TypeEnv (ExceptT TypeError (State InferState)) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadError TypeError
             , MonadState InferState
             , MonadReader TypeEnv
             )

data TypeError
  = UnificationError Type Type
  | UnboundVar String

newtype InferState = InferState { count :: Int }
  deriving Show

newtype TypeEnv = TypeEnv (Map.Map Name Scheme)

lookupEnv :: Name -> TypeInfer Scheme
lookupEnv name = do
  TypeEnv env <- ask
  case Map.lookup name env of
    Nothing -> throwError $ UnboundVar name
    Just s  -> return s

extendEnv :: TypeEnv -> (Name, Scheme) -> TypeEnv
extendEnv (TypeEnv env) (x, s) = TypeEnv $ Map.insert x s env

emptyEnv :: TypeEnv
emptyEnv = TypeEnv Map.empty

newtype Subst = Subst (Map.Map TVar Type) deriving Show

emptySubst :: Subst
emptySubst = Subst Map.empty

makeSubst :: TVar -> Type -> Subst
makeSubst name t = Subst $ Map.singleton name t

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
  apply s (TypeEnv env) = TypeEnv (Map.map (apply s) env)

  ftv (TypeEnv tenv) = ftv $ Map.elems tenv

instance Substitutable a => Substitutable [a] where
  apply s = map $ apply s
  ftv = foldr (Set.union . ftv) Set.empty

instantiation :: Scheme -> TypeInfer Type
instantiation (Forall vars t) = do
  nvars <- mapM (const newTyVar) vars
  let s = Map.fromList (zip vars nvars)
  return $ apply (Subst s) t

generalization :: TypeEnv -> Type -> Scheme
generalization env t  = Forall as t
  where as = Set.toList $ ftv t `Set.difference` ftv env

runTypeInfer :: TypeInfer a -> (Either TypeError a, InferState)
runTypeInfer t = runState (runExceptT $ runReaderT (runTI t) emptyEnv) initTIState
  where initTIState = InferState { count = 0 }

unify :: Type -> Type -> TypeInfer Subst
unify TInt TInt = return emptySubst
unify TBool TBool = return emptySubst
unify (TVar name) t | not (checkOccurs name t) = return $ makeSubst name t
unify t (TVar name) | not (checkOccurs name t) = return $ makeSubst name t
unify (TFun t1 t2) (TFun t1' t2') = do
  s <- unify t1 t1'
  s' <- unify (apply s t2) (apply s t2')
  return $ s' @@ s
unify t1 t2 = throwError $ UnificationError t1 t2

checkOccurs :: TVar -> Type -> Bool
checkOccurs x (TVar name)  = x == name
checkOccurs x (TFun t1 t2) = checkOccurs x t1 || checkOccurs x t2
checkOccurs _ TInt         = False
checkOccurs _ TBool        = False

mAlgorithm :: Expr -> Type -> TypeInfer Subst
mAlgorithm expr expected = case expr of
  EConst (CInt _) -> unify TInt expected

  EConst (CBool _) -> unify TBool expected

  EVar name -> do
    typescheme <- lookupEnv name
    typ <- instantiation typescheme
    unify typ expected

  EAbs name e -> do
    tyenv <- ask
    a1 <- newTyVar
    a2 <- newTyVar
    s <- unify (TFun a1 a2) expected

    let tyenv' = extendEnv (apply s tyenv) (name, Forall [] $ apply s a1)
        a3 = apply s a2

    s' <- local (const tyenv') (mAlgorithm e a3)

    return $ s' @@ s

  EApp e1 e2 -> do
    a <- newTyVar
    s <- mAlgorithm e1 (TFun a expected)
    tyenv <- ask

    let tyenv' = apply s tyenv
        a2 = apply s a

    s' <- local (const tyenv') (mAlgorithm e2 a2)

    return $ s' @@ s

  ELet name e1 e2 -> do
    tyenv <- ask
    a <- newTyVar
    s <- mAlgorithm e1 a

    let tyenv' = apply s tyenv
        expected' = apply s expected
        a' = apply s a

    let typescheme = generalization tyenv' a'
        tyenv'' = extendEnv tyenv' (name, typescheme)
        expected'' = apply s expected'

    s' <- local (const tyenv'') (mAlgorithm e2 expected'')

    return $ s' @@ s

  EIf e1 e2 e3 -> do
    tyenv <- ask
    s <- mAlgorithm e1 TBool

    let tyenv' = apply s tyenv
        expected' = apply s expected

    s' <- local (const tyenv') (mAlgorithm e2 expected')

    let tyenv'' = apply s tyenv'
        expected'' = apply s expected'

    s'' <- local (const tyenv'') (mAlgorithm e3 expected'')

    return $ s'' @@ s' @@ s

  EOp bop e1 e2 -> case bop of
    Add -> binaryop TInt TInt
    Sub -> binaryop TInt TInt
    Mul -> binaryop TInt TInt
    Equal -> do
      a <- newTyVar
      binaryop a TBool
    where
      binaryop op r = do
        tyenv <- ask
        s <- unify r expected

        let tyenv' = apply s tyenv
            op' = apply s op

        s' <- local (const tyenv') (mAlgorithm e1 op')

        let tyenv'' = apply s' tyenv'
            op'' = apply s' op'

        s'' <- local (const tyenv'') (mAlgorithm e2 op'')

        return $ s'' @@ s' @@ s

newTyVar:: TypeInfer Type
newTyVar = do
  s <- get
  put s{count = count s + 1}
  return $ TVar $ TV (letters !! count s)

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

inferExpr :: Expr -> Either TypeError Type
inferExpr expr = fst $ runTypeInfer infer
  where
    infer = do
      a <- newTyVar
      subst <- mAlgorithm expr a
      return $ apply subst a
