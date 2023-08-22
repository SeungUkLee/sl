module SLang.Parser.Error
  ( ParseError (..)
  ) where

import           Data.Text             (Text)
import           Data.Void             (Void)
import           Text.Megaparsec.Error (ParseErrorBundle, errorBundlePretty)

newtype ParseError
  =  ParseError (ParseErrorBundle Text Void)

instance Show ParseError where
  show (ParseError e) = errorBundlePretty e
