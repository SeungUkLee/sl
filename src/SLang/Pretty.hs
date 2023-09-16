{-# LANGUAGE FlexibleInstances #-}

module SLang.Pretty
  ( parensIf
  , renderIO
  , toText
  , Pretty (..)
  ) where

import qualified Data.Text                 as T
import           Prettyprinter             (Doc)
import qualified Prettyprinter             as PP
import qualified Prettyprinter.Render.Text as PP.Text
import           System.IO                 (Handle)

class Pretty a where
  pretty :: a -> Doc ann

instance Pretty Integer where
  pretty = PP.pretty

instance Pretty String where
  pretty = PP.pretty

instance Pretty T.Text where
  pretty = PP.pretty

parensIf :: Bool -> Doc ann -> Doc ann
parensIf True  = PP.parens
parensIf False = id

renderIO :: Handle -> Doc ann -> IO ()
renderIO handle doc = PP.Text.renderIO handle $ PP.layoutPretty PP.defaultLayoutOptions doc

toText :: Doc ann -> T.Text
toText doc = PP.Text.renderStrict $ PP.layoutPretty PP.defaultLayoutOptions doc
