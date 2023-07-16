module Pretty where

import           Prettyprinter

import           Syntax
import           TypeInfer

parensIf ::  Bool -> Doc ann  -> Doc ann
parensIf True  = parens
parensIf False = id

pprExpr :: Expr -> Doc ann
pprExpr (EConst (CInt n)) = pretty n
pprExpr (EConst (CBool b)) = pretty b
pprExpr (EVar name) = pretty name
pprExpr (EApp func arg) = pprExpr func  <+> pprExpr arg
pprExpr (EAbs name body) = pretty "fun" <+> pretty name <+> pprExpr body
pprExpr (ELet name evalue body) = pretty "let" <+> pretty name <+> pprExpr evalue <+> pprExpr body
pprExpr (EIf cond th el) = pretty "if" <+> pprExpr cond <+> pretty "then" <+> pprExpr th <+> pretty "else" <+> pprExpr el
pprExpr (EOp bop e1 e2) = pprExpr e1 <+> pprBop bop <+> pprExpr e2

pprBop :: Bop -> Doc ann
pprBop Add   = pretty "+"
pprBop Sub   = pretty "-"
pprBop Mul   = pretty "*"
pprBop Equal = pretty "=="

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

showExpr :: Expr -> String
showExpr = show . pprExpr

showBop :: Bop -> String
showBop = show . pprBop

showType :: Type -> String
showType = show . pprType

showTVar :: TVar -> String
showTVar = show . pprTVar

showScheme :: Scheme -> String
showScheme = show . pprScheme

showTypeSig :: (String, Scheme) -> String
showTypeSig (name, scheme) = name ++ " : " ++ showScheme scheme

-- TODO: Oprhan instance warning
instance Show TypeError where
  show (UnboundVar name) = "[Type error] unbound variable : " ++ name
  show (UnificationError t1 t2) = "[Type error] unification error : " ++ showType t1 ++ showType t2
