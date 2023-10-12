{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module SLang.Interative.Repl
  ( loop
  ) where

import           Control.Monad.Catch      (Exception (displayException),
                                           MonadCatch (catch), MonadMask,
                                           SomeException (SomeException))
import           Control.Monad.IO.Class   (MonadIO (liftIO))
import qualified Data.Text                as T
import           SLang.Eval.Class         (SLangEval)
import qualified SLang.Interative.Command as Cmd
import           SLang.Parser.Class       (SLangParser)
import           SLang.Parser.Lexer       (reservedWords)
import qualified SLang.Pretty             as SP
import           SLang.TypeInfer.Class    (SLangTypeInfer)
import           System.Console.Repline   (CompleterStyle (Prefix),
                                           ExitDecision (Exit), HaskelineT,
                                           MultiLine (..), ReplOpts (..),
                                           evalReplOpts, fileCompleter,
                                           listCompleter, wordCompleter)
import qualified System.IO                as IO

import           Data.List                (isPrefixOf)
import qualified Data.Text.IO             as TIO
import           SLang.Interative.Command (executeCmd)
import           System.Exit              (exitSuccess)

loop
  :: ( MonadMask m
     , MonadIO m
     , SLangEval (HaskelineT m)
     , SLangTypeInfer (HaskelineT m)
     , SLangParser (HaskelineT m)
     )
  => m ()
loop = do
  let banner SingleLine = return "sl> "
      banner MultiLine  = return "| "

  let command = dontCrash . execute Cmd.interpret "(input)"

  let help _ = SP.prettyprint IO.stdout helpMessage

  let quit _ = SP.prettyprint IO.stdout helpMessage >> liftIO exitSuccess

  let load = dontCrash . execute (const Cmd.interpretWithFile) ""

  let typeof = dontCrash . execute Cmd.typeinfer "(input)"

  let parse = dontCrash . execute Cmd.parsing "(input)"

  let options =
        [ ("load", load)
        , ("type", typeof)
        , ("quit", quit)
        , ("parse", parse)
        , ("help", help)
        ]

  let prefix = Just ':'
  let multilineCommand = Just ":{"

  let tabComplete = Prefix (wordCompleter byWord) defaultMatcher
        where
          reserved = fmap T.unpack reservedWords

          byWord n = do
            return $ filter (isPrefixOf n) reserved

          optionWords = fmap f_ options
            where
              f_ (optionWord, _) = ":" <> optionWord

          defaultMatcher =
            [ (":type", listCompleter reserved)
            , (":load", fileCompleter)
            , (":quit", listCompleter [])
            , (":parse", listCompleter reserved)
            , (":help", listCompleter [])
            , (":", listCompleter optionWords)
            ]

  let initialiser = SP.prettyprint IO.stdout introMessage

  let finaliser = SP.prettyprint IO.stdout finalMessage >> return Exit

  evalReplOpts ReplOpts { .. }

execute :: (SP.Pretty a, MonadIO m) => (FilePath -> T.Text -> m a) -> FilePath -> String -> m ()
execute cmd file code = executeCmd (cmd file) IO.stdout (T.pack code)

dontCrash :: (MonadIO m, MonadCatch m) => m () -> m ()
dontCrash m = catch m (\(SomeException e) ->
  liftIO $ TIO.hPutStrLn IO.stderr $ T.pack $ displayException e)

introMessage :: T.Text
introMessage = T.unlines
  [ ""
  , "   _____ _                         Welcome to SLang REPL!"
  , "  / ____| |"
  , " | (___ | |     __ _ _ __   __ _   Author:  Seunguk Lee"
  , "  \\___ \\| |    / _` | '_ \\ / _` |  GitHub:  https://github.com/seunguklee/slang"
  , "  ____) | |___| (_| | | | | (_| |  Issues:  https://github.com/seunguklee/issues"
  , " |_____/|______\\__,_|_| |_|\\__, |  About:   ML dialect language with let-ploymorphic type system"
  , "                            __/ |  License: MIT"
  , "                           |___/"
  , ""
  , "Type ':help' for available commands"
  ]

helpMessage :: T.Text
helpMessage = T.unlines
  [ "Commands available from the REPL\n"
  , "   :{\n ..lines.. <Ctrl-D>     multiline command"
  , "   :help                       display this list of commands"
  , "   :load <file>                load file and interpret"
  , "   :parse <expr>               parse <expr> and show the AST of <expr>"
  , "   :quit                       exit REPL"
  , "   :type <expr>                show the type of <expr>"
  ]

finalMessage :: T.Text
finalMessage = "GoodBye!\n"
