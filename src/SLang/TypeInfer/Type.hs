module SLang.TypeInfer.Type
  ( Type (..)
  , Scheme (..)
  , TVar (..)
  ) where

import           Prettyprinter (Doc, Pretty (pretty), hcat, punctuate, space,
                                (<+>))

import           SLang.Pretty  (parensIf)

data Type
  = TInt
  | TBool
  | TVar TVar
  | TFun Type Type
  deriving (Eq, Ord)

newtype TVar = TV String
  deriving (Eq, Ord)

data Scheme = Forall [TVar] Type
  deriving (Eq , Ord)

instance Show Type where
  show = show . pprType

instance Show TVar where
  show = show . pprTVar

instance Show Scheme where
  show = show . pprScheme

pprType :: Type -> Doc ann
pprType TInt                        = pretty "int"
pprType TBool                       = pretty "bool"
pprType (TVar tvar)                 = pprTVar tvar
pprType (TFun inputType returnType) = parensIf (isArrow inputType) (pprType inputType)
  <+> pretty "->" <+> pprType returnType
  where
    isArrow TFun{} = True
    isArrow _      = False

pprTVar :: TVar -> Doc ann
pprTVar (TV name) = pretty name

pprScheme :: Scheme -> Doc ann
pprScheme (Forall [] t) = pprType t
pprScheme (Forall ts t) = pretty "forall" <+> hcat (punctuate space (fmap pprTVar ts)) <+> pretty "."  <+> pprType t
