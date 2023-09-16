{-# LANGUAGE OverloadedStrings #-}

module SLang.TypeInfer.Type
  ( Type (..)
  , Scheme (..)
  , TVar (..)
  ) where

import qualified Data.Text     as T
import           Prettyprinter (Doc, hcat, punctuate, space, (<+>))

import qualified SLang.Pretty  as SP
import           SLang.Pretty  (Pretty)

data Type
  = TInt
  | TBool
  | TVar TVar
  | TFun Type Type
  deriving (Eq, Ord, Show)

newtype TVar = TV T.Text
  deriving (Eq, Ord, Show)

data Scheme = Forall [TVar] Type
  deriving (Eq , Ord, Show)

instance Pretty Type where
  pretty = pprType

instance Pretty TVar where
  pretty = pprTVar

instance Pretty Scheme where
  pretty = pprScheme

pprType :: Type -> Doc ann
pprType TInt                        = "int"
pprType TBool                       = "bool"
pprType (TVar tvar)                 = pprTVar tvar
pprType (TFun inputType returnType) =
  SP.parensIf (isArrow inputType) (pprType inputType) <+> "->" <+> pprType returnType
  where
    isArrow TFun{} = True
    isArrow _      = False

pprTVar :: TVar -> Doc ann
pprTVar (TV name) = SP.pretty name

pprScheme :: Scheme -> Doc ann
pprScheme (Forall [] t) = pprType t
pprScheme (Forall ts t) =
  "forall" <+> hcat (punctuate space (fmap pprTVar ts)) <+> "." <+> pprType t
