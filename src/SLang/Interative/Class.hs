module SLang.Interative.Class
  ( Interative (..)
  ) where

import           Control.Monad.IO.Class        (MonadIO)
import qualified Data.Text                     as T
import           SLang.Interative.Cli.OptParse (Input, Output)
import           SLang.Pretty                  (Pretty)

class (MonadIO m) => Interative m where
  cli :: (Pretty a) => (FilePath -> T.Text -> m a) -> Input -> Output -> m ()
  repl :: m ()
