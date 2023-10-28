{-# LANGUAGE FlexibleInstances #-}

module SLang.Pretty
  ( parensIf
  , renderIO
  , toText
  , prettyprint

  , Pretty (..)
  ) where

import           Control.Monad.IO.Class    (MonadIO (liftIO))
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

prettyprint :: (MonadIO m, Pretty a) => Handle -> a -> m ()
prettyprint o a = liftIO $ renderIO o $ pretty a
