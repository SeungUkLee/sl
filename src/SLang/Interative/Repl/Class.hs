module SLang.Interative.Repl.Class
  ( SLangRepl (..)
  ) where

import           Control.Monad.Catch
import           Control.Monad.Except
import qualified System.Console.Haskeline as H

class (MonadIO m, MonadMask m) => SLangRepl m where
  getInputLine :: String -> m (Maybe String)
  outputStrLn :: String -> m ()

instance (MonadIO m, MonadMask m) => SLangRepl (H.InputT m) where
  getInputLine = H.getInputLine
  outputStrLn = H.outputStrLn
