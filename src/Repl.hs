{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Repl where

import qualified Data.Text              as T

import           Control.Monad.State
import           Data.Bifunctor         (second)
import           Data.List              (isPrefixOf)
import           Eval                   (evalExpr)
import           Parser                 (parseSL)
import           Pretty                 (showType)
import           System.Console.Repline (Cmd, CompleterStyle (Word0),
                                         ExitDecision (Exit), HaskelineT,
                                         Options,
                                         ReplOpts (ReplOpts, banner, command, finaliser, initialiser, multilineCommand, options, prefix, tabComplete),
                                         WordCompleter, abort, evalReplOpts)
import           TypeInfer              (inferExpr)

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
  ast <- hoistError $ parseSL code
  typ <- hoistError $ fst (inferExpr ast)
  res <- liftIO $ evalExpr ast
  val <- hoistError res
  liftIO $ putStrLn $ show val ++ " : " ++ showType typ
  where
    hoistError :: Show e => Either e a -> Repl a
    hoistError (Right v) = return v
    hoistError (Left err) = do
      liftIO $ print err
      Repl abort

opts :: Options Repl
opts =
  [ ("help", help)
  ]

completer :: Monad m => WordCompleter m
completer n = do
  let cmds = [":type", ":load"]
  return $ filter (isPrefixOf n) cmds

help :: Cmd Repl
help args = liftIO $ print $ "Help: " ++ show args
