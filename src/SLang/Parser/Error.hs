module SLang.Parser.Error
  ( ParseError (..)
  ) where

import           Control.Exception     (Exception (displayException))
import           Data.Text             (Text)
import           Data.Void             (Void)

import           Text.Megaparsec.Error (ParseErrorBundle, errorBundlePretty)

newtype ParseError
  =  ParseError (ParseErrorBundle Text Void)
  deriving (Show, Eq)

instance Exception ParseError where
  displayException (ParseError e) = errorBundlePretty e
