{-# LANGUAGE ConstrainedClassMethods #-}

module SLang.Interative.Cli.Class
  ( SLangCli (..)
  , InputHandle (..)
  , OutputHandle (..)
  ) where

import           Control.Monad.IO.Class (MonadIO)
import qualified Data.Text              as T
import           SLang.Pretty           (Pretty)
import           System.IO              (Handle)

class (MonadIO m) => SLangCli m where

  cli :: (Pretty a) => (FilePath -> T.Text -> m a) -> m (InputHandle m) -> m (OutputHandle m) -> m ()

  inputFile :: T.Text -> m (InputHandle m)
  outputFile :: T.Text -> m (OutputHandle m)
  stdin :: m (InputHandle m)
  stdout :: m (OutputHandle m)


newtype InputHandle m = InputHandle
  { unInputHandle :: ((Handle -> m ()) -> m (), FilePath) }

newtype OutputHandle m = OutputHandle
  { unOutputHandle :: (Handle -> m ()) -> m () }
