{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module SLang.REPL
  ( mainLoop
  ) where

import qualified Data.Text              as T

import           Data.Bifunctor         (second)
import           Data.List              (isPrefixOf)
import qualified Data.Text.IO           as TIO
import           System.Console.Repline (Cmd, CompleterStyle (Prefix),
                                         CompletionFunc, ExitDecision (Exit),
                                         HaskelineT,
                                         MultiLine (MultiLine, SingleLine),
                                         Options,
                                         ReplOpts (ReplOpts, banner, command, finaliser, initialiser, multilineCommand, options, prefix, tabComplete),
                                         WordCompleter, abort, evalReplOpts,
                                         fileCompleter, listCompleter,
                                         wordCompleter)
import           System.Exit            (exitSuccess)

import           Control.Exception      (Exception (displayException))
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           SLang.Eval             (evalExpr)
import           SLang.Parser           (parseToExpr, reservedWords)
import qualified SLang.Pretty           as SP
import qualified SLang.Result           as Result
import           SLang.TypeInfer        (inferExpr)
import qualified System.IO              as IO

newtype Repl a = Repl
  { runRepl :: HaskelineT IO a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             )

mainLoop :: IO ()
mainLoop = evalReplOpts $ ReplOpts
  { banner           = runRepl . customBanner
  , command          = runRepl . cmd
  , options          = map (second (runRepl .)) opts -- ^ map (\(s, repl) -> (s, unRepl . repl)) opts
  , prefix           = Just ':'
  , multilineCommand = Just "{"
  , tabComplete      = Prefix (wordCompleter byWord) defaultMatcher
  , initialiser      = runRepl ini
  , finaliser        = runRepl final
  }

customBanner :: MultiLine -> Repl String
customBanner SingleLine = pure "sl> "
customBanner MultiLine  = pure "| "

final :: Repl ExitDecision
final = do
  liftIO $ TIO.putStrLn goodByeMsg
  return Exit

ini :: Repl ()
ini = liftIO $ TIO.putStrLn introMsg

goodByeMsg :: T.Text
goodByeMsg = "GoodBye!\n"

introMsg :: T.Text
introMsg = T.unlines
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

cmd :: String -> Repl ()
cmd code = process (T.pack code)

process :: T.Text -> Repl ()
process code = do
  ast <- hoistError $ parseToExpr "(input)" code
  typ <- hoistError $ inferExpr ast
  val <- hoistError $ evalExpr ast
  liftIO $ SP.renderIO IO.stdout $ SP.pretty $ Result.Interpret typ val

hoistError :: Exception e => Either e a -> Repl a
hoistError (Right v) = return v
hoistError (Left err) = do
  liftIO $ TIO.putStrLn $ T.pack $ displayException err
  Repl abort

opts :: Options Repl
opts =
  [ ("load", load)
  , ("type", typeof)
  , ("quit", quit)
  , ("parse", parse)
  , ("help", help)
  ]

byWord :: Monad m => WordCompleter m
byWord n = do
  return $ filter (isPrefixOf n) reserved

defaultMatcher :: MonadIO m => [(String, CompletionFunc m)]
defaultMatcher =
  [ (":type", listCompleter reserved)
  , (":load", fileCompleter)
  , (":quit", listCompleter [])
  , (":parse", listCompleter reserved)
  , (":help", listCompleter [])
  , (":", listCompleter cmds)
  ]

reserved :: [String]
reserved = fmap T.unpack reservedWords

cmds :: [String]
cmds = [":type" , ":load" , ":quit" , ":parse" , ":help"]

typeof :: Cmd Repl
typeof code = do
  ast <- hoistError $ parseToExpr "(input)" (T.pack code)
  typ <- hoistError $ inferExpr ast
  liftIO $ SP.renderIO IO.stdout $ SP.pretty $ Result.TypeInfer ast typ

load :: Cmd Repl
load file = do
  code <- liftIO $ TIO.readFile file
  process code

parse :: Cmd Repl
parse code = do
  ast <- hoistError $ parseToExpr "(input)" (T.pack code)
  liftIO $ SP.renderIO IO.stdout $ SP.pretty $ Result.Parse ast

help :: Cmd Repl
help _ = do
  liftIO $ TIO.putStrLn helpMsg

helpMsg :: T.Text
helpMsg = T.unlines
  [ "Commands available from the REPL\n"
  , "   :{\n ..lines.. <Ctrl-D>     multiline command"
  , "   :help                       display this list of commands"
  , "   :load <file>                load file and interpret"
  , "   :parse <expr>               parse <expr> and show the AST of <expr>"
  , "   :quit                       exit REPL"
  , "   :type <expr>                show the type of <expr>"
  ]

quit :: Cmd Repl
quit _ = do
  liftIO $ TIO.putStrLn goodByeMsg
  liftIO exitSuccess
