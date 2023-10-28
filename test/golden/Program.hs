module Program
  ( interpretWithW
  , interpretWithM
  ) where

import           Data.Text
import           SLang

interpretWithW, interpretWithM :: (Command m, Algorithm m) => FilePath -> Text -> m (Value, Type)
interpretWithW = interpret algorithmW
interpretWithM = interpret algorithmM
