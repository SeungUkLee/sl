module SLang.Pretty
  ( parensIf
  ) where

import           Prettyprinter (Doc, parens)

parensIf ::  Bool -> Doc ann  -> Doc ann
parensIf True  = parens
parensIf False = id
