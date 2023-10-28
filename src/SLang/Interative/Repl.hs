{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module SLang.Interative.Repl
  ( -- * re-exports
    module SLang.Interative.Repl.Class
  , module SLang.Interative.Repl.Error

  , ReplConfig (..)

  , runSLangRepl
  , runSLangReplWithConfig
  , defaultReplConfig
  , executeReplCmd
  )
where

import           Control.Monad                 ((>=>))
import           Control.Monad.Catch           (Exception (displayException),
                                                Handler (Handler),
                                                MonadThrow (throwM),
                                                SomeException (SomeException),
                                                catches)
import           Control.Monad.IO.Class        (MonadIO (liftIO))
import qualified Data.Text                     as T

import qualified Data.Text.IO                  as TIO

import           SLang.Interative.Repl.Class
import           SLang.Interative.Repl.Error
import           SLang.Interative.Repl.Message (finalMessage, helpMessage,
                                                introMessage)
import qualified SLang.Pretty                  as SP
import           SLang.Program                 (Algorithm (..),
                                                Command (interpret, parsing, typeinfer))
import           System.Exit                   (ExitCode (..), exitSuccess)

data ReplConfig m = ReplConfig
  { banner        :: m String
  , process       :: ReplProcessCommand m
  , commands      :: ReplCommands m
  , commandPrefix :: Maybe Char
  , initialiser   :: m ()
  , finaliser     :: m ()
  }

type ReplProcessCommand m = T.Text -> m ()
type ReplCommands m = [(T.Text, ReplCommand m)]
type ReplCommand m = T.Text -> m ()

runSLangReplWithConfig :: (SLangRepl m) => ReplConfig m -> m ()
runSLangReplWithConfig config = do
  let ReplConfig { .. } = config
  initialiser >> replLoop banner process commands commandPrefix finaliser

runSLangRepl :: (SLangRepl m, Algorithm m, Command m) => m ()
runSLangRepl = runSLangReplWithConfig defaultReplConfig

replLoop
  :: ( SLangRepl m
     )
  =>  m String
  -- ^ banner
  -> ReplProcessCommand m
  -- ^ process command
  -> ReplCommands m
  -- ^ commands
  -> Maybe Char
  -- ^ command prefix
  -> m ()
  -- ^ finaliser
  -> m ()
replLoop banner prCmd cmds cmdPrefix finalz = loop
  where
    loop = do
      prefix <- banner
      userInput <- getInputLine prefix
      case userInput of
        Nothing -> finalz
        Just "" -> loop
        Just s -> catches (do
          (cmdTxt, arg_) <- pReplCmd cmdPrefix (T.strip $ T.pack s)
          cmd <- if cmdTxt == "" then return prCmd else getReplCmd cmds cmdTxt
          cmd arg_ >> loop)

          [ Handler $ \case
              ExitSuccess -> outputStrLn $ T.unpack finalMessage
              ExitFailure  _ -> return ()
          , Handler $ \(SomeException e) -> do
              outputStrLn $ displayException e
              loop
          ]

pReplCmd :: (MonadThrow m) => Maybe Char -> T.Text -> m (T.Text, T.Text)
pReplCmd Nothing txt = return ("", txt)
pReplCmd (Just prefix) txt
  | prefix == T.head txt =
    let excludedPrefix = T.tail txt in
    if null $ T.unpack excludedPrefix then throwM $ EmptyCommand $ T.pack [prefix]
    else
      let wordsList = T.words excludedPrefix
          cmdTxt = head wordsList
          argTxt = T.unwords $ tail wordsList
      in return (cmdTxt, argTxt)
  | otherwise = return ("", txt)

getReplCmd :: (MonadThrow m) => ReplCommands m -> T.Text -> m (T.Text -> m ())
getReplCmd [] s = throwM $ NoSuchCommand s
getReplCmd ((cmdName, cmd) : replCmds) s
  | s `T.isPrefixOf` cmdName = return cmd
  | otherwise = getReplCmd replCmds s

executeReplCmd :: (SLangRepl m, SP.Pretty b) => (a -> m b) -> a -> m ()
executeReplCmd cmd = cmd >=> outputStrLn . show . SP.pretty

defaultReplConfig :: (SLangRepl m, Algorithm m, Command m) => ReplConfig m
defaultReplConfig = ReplConfig
  { banner = return "sl> "
  , process = executeReplCmd (interpret algorithmM "(input)")
  , commands = replCommands
  , commandPrefix = Just ':'
  , initialiser = outputStrLn $ T.unpack introMessage
  , finaliser = outputStrLn $ T.unpack finalMessage
  }

replCommands :: (SLangRepl m, Command m, Algorithm m) => ReplCommands m
replCommands =
  [ ("quit", quit)
  , ("help", help)
  , ("parse", executeReplCmd (parsing "(input)"))
  , ("load", executeReplCmd interpretWithFile)
  , ("type", executeReplCmd (typeinfer algorithmM "(input)"))
  ]
  where
    quit _ = do
      liftIO exitSuccess

    help _ = do
      outputStrLn $ T.unpack helpMessage

    interpretWithFile file = do
      let striped = T.unpack $ T.strip file

      code <- liftIO $ TIO.readFile striped
      interpret algorithmM striped code
