{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SLang
  ( -- * re-exports
    module SLang.Eval
  , module SLang.Parser
  , module SLang.Pretty
  , module SLang.REPL
  , module SLang.Result
  , module SLang.TypeInfer

  , main
  , execEval
  , execParser
  , execTypeInfer
  , SLangError (..)
  ) where

import           Control.Exception    (Exception (displayException),
                                       Handler (Handler),
                                       SomeException (SomeException), catches,
                                       throwIO)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           Options.Applicative  (CommandFields, Mod, Parser, ParserInfo,
                                       command, fullDesc, header, help, helper,
                                       info, long, metavar, optional, progDesc,
                                       short, strOption, subparser, (<|>))
import qualified Options.Applicative  as Opt

import           SLang.Eval           (EvalError, Expr, Value, evalExpr)
import           SLang.Parser         (ParseError, parseToExpr)
import           SLang.Pretty
import           SLang.REPL           (mainLoop)
import           SLang.Result
import           SLang.TypeInfer      (TypeError, inferExpr)

import           Data.Maybe           (fromMaybe)
import qualified SLang.Pretty         as SP
import qualified SLang.Result         as Result
import           SLang.TypeInfer.Type (Type)
import           System.Directory     (doesFileExist)
import           System.Exit          (ExitCode (ExitFailure, ExitSuccess),
                                       exitFailure)
import qualified System.IO            as IO
import           System.IO            (Handle, IOMode (ReadMode, WriteMode))

data SLangError
  = ParseError ParseError
  | EvalError EvalError
  | TypeError TypeError
  deriving Show

instance Exception SLangError where
  displayException (ParseError e) = displayException e
  displayException (EvalError e)  = displayException e
  displayException(TypeError e)   = displayException e

data Options
  = InterpretOpt Input Output
  | ParseOpt Input Output
  | TypeOfOpt Input Output
  | REPLOpt
  deriving Show

data Input
  = Stdin
  | InputFile FilePath
  deriving Show

data Output
  = Stdout
  | OutputFile FilePath
  deriving Show


-- | Command line entry point
main :: IO ()
main = do
  catches (do
    options <- optParse
    case options of
      InterpretOpt input output -> actionWithIOHandle interpret input output
      ParseOpt input output     -> actionWithIOHandle parse input output
      TypeOfOpt input output    -> actionWithIOHandle typeof input output
      REPLOpt                   -> mainLoop
    )
    [ Handler $ \case
        ParseError e -> TIO.hPutStrLn IO.stderr $ T.pack (displayException e)
        TypeError e -> TIO.hPutStrLn IO.stderr $ T.pack (displayException e)
        EvalError e -> TIO.hPutStrLn IO.stderr $ T.pack (displayException e)
    , Handler $ \case
        ExitSuccess -> return ()
        ExitFailure _ -> return ()
    , Handler $ \(SomeException e) -> TIO.hPutStrLn IO.stderr $ T.pack (displayException e)
    ]

actionWithIOHandle :: (FilePath -> Handle -> Handle -> IO a) -> Input -> Output -> IO a
actionWithIOHandle action input output =
  withInputHandle (\file -> flip withOutputHandle output . action file) input

withInputHandle :: (FilePath -> Handle -> IO a) -> Input ->  IO a
withInputHandle action input =
  case input of
    Stdin          -> action "(input)" IO.stdin
    InputFile file -> IO.withFile file ReadMode $ action file

withOutputHandle :: (Handle -> IO a) -> Output -> IO a
withOutputHandle action output =
  case output of
    Stdout -> action IO.stdout
    OutputFile file -> do
      exists <- doesFileExist file
      shouldOpenFile <- if exists then confirm else pure True
      if shouldOpenFile
        then IO.withFile file WriteMode action
        else exitFailure

confirm :: IO Bool
confirm = do
  TIO.putStrLn "This file alredy exists. Do you want to overwrite it? (y/n)"
  answer <- getLine
  case answer of
    "y" -> pure True
    "n" -> pure False
    _   -> TIO.putStrLn "Please use 'y' or 'n'" *> confirm

pOptions :: Parser Options
pOptions = subparser $ pInterpretCommand <> pParsingCommand <> pTypeOfCommand <> pREPLCommand

pInterpretInfo :: ParserInfo Options
pInterpretInfo = info
  (helper <*> pInterpret)
  (progDesc "Interpret a SLang file")

pParsingInfo :: ParserInfo Options
pParsingInfo = info
  (helper <*> pParsing)
  (progDesc "Parse a SLang file")

pTypeOfInfo :: ParserInfo Options
pTypeOfInfo = info
  (helper <*> pTypeOf)
  (progDesc "Type Inference a SLang file")

pREPLInfo :: ParserInfo Options
pREPLInfo = info
  (helper <*> pREPL)
  (progDesc "Enter a REPL for SLang")

pInterpret :: Parser Options
pInterpret = InterpretOpt <$> pInput <*> pOutput

pInput :: Parser Input
pInput = fromMaybe Stdin <$> optional pInputFile

pInputFile :: Parser Input
pInputFile = fmap InputFile parser
  where
    parser = strOption
      ( long "input"
      <> short 'i'
      <> metavar "FILE"
      <> help "Input file"
      )

pOutput :: Parser Output
pOutput = fromMaybe Stdout <$> optional pOutputFile

pOutputFile :: Parser Output
pOutputFile = fmap OutputFile parser
  where
    parser = strOption
      ( long "output"
      <> short 'o'
      <> metavar "FILE"
      <> help "Output file"
      )

pParsing :: Parser Options
pParsing = ParseOpt <$> pInput <*> pOutput

pTypeOf :: Parser Options
pTypeOf = TypeOfOpt <$> pInput <*> pOutput

pREPL :: Parser Options
pREPL = do
  pure REPLOpt

pInterpretCommand :: Mod CommandFields Options
pInterpretCommand = command "interpret" pInterpretInfo

pParsingCommand :: Mod CommandFields Options
pParsingCommand = command "parse" pParsingInfo

pTypeOfCommand :: Mod CommandFields Options
pTypeOfCommand = command "type" pTypeOfInfo

pREPLCommand :: Mod CommandFields Options
pREPLCommand = command "repl" pREPLInfo

pOptsInfo :: ParserInfo Options
pOptsInfo = info
  (helper <*> pOptions <|> pREPL)
  (fullDesc
  <> header "SLang - a simple language"
  <> progDesc "Command line for SLang"
  )

optParse :: IO Options
optParse = Opt.execParser pOptsInfo

interpret :: FilePath -> Handle -> Handle -> IO ()
interpret file input output = do
  code <- IO.hGetContents input
  ast <- execParser file (T.pack code)
  typ <- execTypeInfer ast
  val <- execEval ast
  SP.renderIO output $ SP.pretty $ Result.Interpret typ val

parse :: FilePath -> Handle -> Handle -> IO ()
parse file input output = do
  code <- IO.hGetContents input
  ast <- execParser file (T.pack code)
  SP.renderIO output $ SP.pretty $ Result.Parse ast

typeof :: FilePath -> Handle -> Handle -> IO ()
typeof file input output = do
  code <- IO.hGetContents input
  ast <- execParser file (T.pack code)
  typ <- execTypeInfer ast
  SP.renderIO output $ SP.pretty $ Result.TypeInfer ast typ

execParser :: FilePath -> T.Text -> IO Expr
execParser file txt = do
  let expr = parseToExpr file txt
  case expr of
    Left e  -> throwIO $ ParseError e
    Right v -> return v

execEval :: Expr -> IO Value
execEval expr = do
  let value = evalExpr expr
  case value of
    Left e  -> throwIO $ EvalError e
    Right v -> return v

execTypeInfer :: Expr -> IO Type
execTypeInfer expr = do
  let typ = inferExpr expr
  case typ of
    Left e  -> throwIO $ TypeError e
    Right v -> return v
