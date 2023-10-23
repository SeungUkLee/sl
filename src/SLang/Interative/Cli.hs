{-# LANGUAGE OverloadedStrings #-}

module SLang.Interative.Cli
  ( -- * re-exports
    module SLang.Interative.Cli.OptParse
    
  , InputHandle (..)
  , OutputHandle (..)

  , getStdinHandle
  , getStdoutHandle
  , getInputFileHandle
  , getOutputFileHandle

  , actionWithIOHandle
  , withInputHandle
  , withOutputHandle
  , openFile
  , hClose
  , confirm
  ) where

import           Control.Monad.Catch           (MonadMask, bracket)
import           Control.Monad.IO.Class        (MonadIO (liftIO))
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           SLang.Interative.Cli.OptParse
import           System.Directory              (doesFileExist)
import           System.Exit                   (exitFailure)
import qualified System.IO                     as IO
import           System.IO                     (Handle)

getInputFileHandle :: (MonadMask m, MonadIO m) => T.Text -> m (InputHandle m)
getInputFileHandle file = do
  let filepath = T.unpack file

  return $ InputHandle (bracket (openFile filepath IO.ReadMode) hClose, filepath)

getStdinHandle :: (Monad m) => m (InputHandle m)
getStdinHandle = do
  return $ InputHandle (\act -> act IO.stdin, "(input)")

getStdoutHandle :: (Monad m) => m (OutputHandle m)
getStdoutHandle = do
  return $ OutputHandle (\act -> act IO.stdout)

getOutputFileHandle :: (MonadMask m, MonadIO m) => T.Text -> m (OutputHandle m)
getOutputFileHandle filepath = do
  let file = T.unpack filepath
  exists <- liftIO $ doesFileExist file
  shouldOpenFile <- if exists then confirm else return True
  if shouldOpenFile
    then return $ OutputHandle (bracket (openFile file IO.WriteMode) hClose)
    else liftIO exitFailure

actionWithIOHandle :: (Handle -> Handle -> m a) -> ((Handle -> m a) -> m a) -> ((Handle -> m a) -> m a) -> m a
actionWithIOHandle action i o = withInputHandle i (withOutputHandle o . action)

withInputHandle :: ((Handle -> m a) -> m a) -> (Handle -> m a) -> m a
withInputHandle = id

withOutputHandle
  :: ((Handle -> m a) -> m a)
  -> (Handle -> m a)
  -- ^ action
  -> m a
withOutputHandle = id

confirm :: (MonadIO m) => m Bool
confirm = do
  liftIO $ TIO.putStrLn "This file alredy exists. Do you want to overwrite it? (y/n)"
  answer <- liftIO getLine
  case answer of
    "y" -> pure True
    "n" -> pure False
    _   -> liftIO $ TIO.putStrLn "Please use 'y' or 'n'" *> confirm

openFile :: (MonadIO m) => FilePath -> IO.IOMode -> m Handle
openFile file mode = liftIO $ IO.openFile file mode

hClose :: (MonadIO m) => Handle -> m ()
hClose h = liftIO $ IO.hClose h

newtype InputHandle m = InputHandle
  { unInputHandle :: ((Handle -> m ()) -> m (), FilePath) }

newtype OutputHandle m = OutputHandle
  { unOutputHandle :: (Handle -> m ()) -> m () }
