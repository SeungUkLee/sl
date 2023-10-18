module Program
  ( interpretWithW
  , interpretWithM
  ) where

import           Data.Text
import           Interpreter
import           SLang

interpretWithW, interpretWithM :: FilePath -> Text -> SLangGoldenTest Result
interpretWithW = interpret algorithmW
interpretWithM = interpret algorithmM
