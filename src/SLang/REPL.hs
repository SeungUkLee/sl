{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module SLang.REPL
  ( mainLoop
  ) where

import qualified Data.Text              as T

import           Control.Monad.State    (MonadIO (..))
import           Data.Bifunctor         (second)
import           Data.List              (isPrefixOf)
import qualified Data.Text.IO           as TIO
import           System.Console.Repline (Cmd, CompleterStyle (Word0),
                                         ExitDecision (Exit), HaskelineT,
                                         Options,
                                         ReplOpts (ReplOpts, banner, command, finaliser, initialiser, multilineCommand, options, prefix, tabComplete),
                                         WordCompleter, abort, evalReplOpts)
import           System.Exit            (exitSuccess)

import           SLang.Eval             (evalExpr)
import           SLang.Parser           (parseToExpr)
import           SLang.TypeInfer        (inferExpr)

newtype Repl a = Repl
  { runRepl :: HaskelineT IO a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             )

mainLoop :: IO ()
mainLoop = evalReplOpts $ ReplOpts
  { banner           = const (pure "sl> ")
  , command          = runRepl . cmd
  , options          = map (second (runRepl .)) opts -- ^ map (\(s, repl) -> (s, unRepl . repl)) opts
  , prefix           = Just ':'
  , multilineCommand = Nothing
  , tabComplete      = Word0 completer
  , initialiser      = runRepl ini
  , finaliser        = runRepl final
  }

final :: Repl ExitDecision
final = do
  liftIO $ putStrLn "GoodBye!\n"
  return Exit

ini :: Repl ()
ini = liftIO $ putStrLn "Welcome!\n"

cmd :: String -> Repl ()
cmd code = process (T.pack code)

process :: T.Text -> Repl ()
process code = do
  ast <- hoistError $ parseToExpr "(input)" code
  typ <- hoistError $ inferExpr ast
  res <- liftIO $ evalExpr ast
  val <- hoistError res
  liftIO $ putStrLn $ show val ++ " : " ++ show typ

hoistError :: Show e => Either e a -> Repl a
hoistError (Right v) = return v
hoistError (Left err) = do
  liftIO $ print err
  Repl abort

opts :: Options Repl
opts =
  [ ("load", load)
  , ("type", typeof)
  , ("quit", quit)
  ]

completer :: Monad m => WordCompleter m
completer n = do
  let cmds = [":type", ":load", ":quit"]
  return $ filter (isPrefixOf n) cmds

typeof :: Cmd Repl
typeof code = do
  ast <- hoistError $ parseToExpr "(input)" (T.pack code)
  typ <- hoistError $ inferExpr ast
  liftIO $ putStrLn $ show code ++ " : " ++ show typ

load :: Cmd Repl
load file = do
  code <- liftIO $ TIO.readFile file
  process code

quit :: Cmd Repl
quit _ = liftIO exitSuccess
