{-# LANGUAGE OverloadedStrings #-}

module SLang.Interative.Cli
  ( -- * re-exports
    module SLang.Interative.Cli.OptParse

  , executeCli
  )
where

import           Control.Monad.Catch           (MonadMask, bracket)
import           Control.Monad.IO.Class        (MonadIO (..))

import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           SLang.Interative.Cli.OptParse (Input (..), Options (..),
                                                Output (..), optParse)
import qualified SLang.Pretty                  as SP
import           System.Directory              (doesFileExist)
import           System.Exit                   (exitFailure)
import qualified System.IO                     as IO
import           System.IO                     (Handle,
                                                IOMode (ReadMode, WriteMode))

executeCli :: (MonadMask m, MonadIO m, SP.Pretty a) => (FilePath -> T.Text -> m a) -> Input -> Output -> m ()
executeCli cmd = actionWithIOHandle action
  where
    action file i o = do
      code <- liftIO $ TIO.hGetContents i
      res <- cmd file code
      liftIO $ SP.renderIO o $ SP.pretty res

actionWithIOHandle :: (MonadIO m, MonadMask m) => (FilePath -> Handle -> Handle -> m a) -> Input -> Output -> m a
actionWithIOHandle action input output =
  withInputHandle (\file -> flip withOutputHandle output . action file) input

withInputHandle :: (MonadMask m, MonadIO m) => (FilePath -> Handle -> m a) -> Input -> m a
withInputHandle action input =
  case input of
    Stdin          -> action "(input)" IO.stdin
    InputFile file -> bracket (openFile file ReadMode) hClose $ action file

openFile :: (MonadIO m) => FilePath -> IOMode -> m Handle
openFile file mode = liftIO $ IO.openFile file mode

hClose :: (MonadIO m) => Handle -> m ()
hClose h = liftIO $ IO.hClose h

withOutputHandle :: (MonadIO m, MonadMask m) => (Handle -> m a) -> Output -> m a
withOutputHandle action output =
  case output of
    Stdout -> action IO.stdout
    OutputFile file -> do
      exists <- liftIO $ doesFileExist file
      shouldOpenFile <- if exists then liftIO confirm else pure True
      if shouldOpenFile
        then bracket (openFile file WriteMode) hClose action
        else liftIO exitFailure

confirm :: IO Bool
confirm = do
  TIO.putStrLn "This file alredy exists. Do you want to overwrite it? (y/n)"
  answer <- getLine
  case answer of
    "y" -> pure True
    "n" -> pure False
    _   -> TIO.putStrLn "Please use 'y' or 'n'" *> confirm
