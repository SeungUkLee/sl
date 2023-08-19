module SLang.Parser.Common
  ( Parser
  , ParseError
  ) where

import           Data.Text             (Text)
import           Data.Void             (Void)
import           Text.Megaparsec       (Parsec)
import           Text.Megaparsec.Error (ParseErrorBundle)

type Parser = Parsec Void Text
type ParseError =  ParseErrorBundle Text Void
