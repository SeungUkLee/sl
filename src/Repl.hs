module Repl where

import           Data.Text                as T

import           Control.Monad.Trans      (MonadIO (liftIO))
import           System.Console.Haskeline (InputT, defaultSettings,
                                           getInputLine, outputStrLn, runInputT)

import           Eval                     (eval)
import           Parser                   (parseSL)

type Repl a = InputT IO a

mainLoop :: IO ()
mainLoop = runInputT defaultSettings repl

repl :: Repl ()
repl = do
  minput <- getInputLine "sl> "
  case minput of
    Nothing    -> outputStrLn "Goodbye!"
    Just input -> liftIO (process input) >> repl

process :: String -> IO ()
process str = do
  res <- case parseSL $ T.pack str of
    Left _  -> return Nothing
    Right a -> return $ eval a
  print res
