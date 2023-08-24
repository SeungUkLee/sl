{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SLang
  ( main
  , execEval
  , execParser
  , execTypeInfer
  , SLangError (..)
  ) where

import           Control.Exception     (Exception, Handler (Handler),
                                        SomeException (SomeException), catches,
                                        throwIO)
import qualified Data.Text             as T
import           Options.Applicative   (CommandFields, Mod, Parser, ParserInfo,
                                        command, fullDesc, header, help, helper,
                                        info, long, metavar, optional, progDesc,
                                        short, strOption, subparser, (<|>))
import qualified Options.Applicative   as Opt

import           SLang.Eval            (evalExpr)
import           SLang.Eval.Error      (EvalError)
import           SLang.Eval.Syntax     (Expr)
import           SLang.Parser          (ParseError, parseToExpr)
import           SLang.REPL            (mainLoop)
import           SLang.TypeInfer       (inferExpr)
import           SLang.TypeInfer.Error (TypeError)

import           Data.Maybe            (fromMaybe)
import           SLang.Eval.Domain     (Value)
import           SLang.TypeInfer.Type  (Type)
import           System.Directory      (doesFileExist)
import           System.Exit           (exitFailure)
import           System.IO             (Handle, IOMode (ReadMode, WriteMode),
                                        hGetContents, hPrint, hPutStrLn, stderr,
                                        stdin, stdout, withFile)

data SLangError
  = ParseError ParseError
  | EvalError EvalError
  | TypeError TypeError
  deriving Show

instance Exception SLangError

data Options
  = Interpret Input Output
  | Parse Input Output
  | TypeOf Input Output
  | REPL
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
      Interpret input output -> actionWithIOHandle interpret input output
      Parse input output     -> actionWithIOHandle parse input output
      TypeOf input output    -> actionWithIOHandle typeof input output
      REPL                   -> mainLoop
    )
    [ Handler $ \case
        ParseError e -> hPrint stderr e
        TypeError e -> hPrint stderr e
        EvalError e -> hPrint stderr e
    , Handler $ \(SomeException e) -> hPrint stderr e
    ]

actionWithIOHandle :: (FilePath -> Handle -> Handle -> IO a) -> Input -> Output -> IO a
actionWithIOHandle action input output =
  withInputHandle (\file -> flip withOutputHandle output . action file) input

withInputHandle :: (FilePath -> Handle -> IO a) -> Input ->  IO a
withInputHandle action input =
  case input of
    Stdin          -> action "(input)" stdin
    InputFile file -> withFile file ReadMode $ action file

withOutputHandle :: (Handle -> IO a) -> Output -> IO a
withOutputHandle action output =
  case output of
    Stdout -> action stdout
    OutputFile file -> do
      exists <- doesFileExist file
      shouldOpenFile <- if exists then confirm else pure True
      if shouldOpenFile
        then withFile file WriteMode action
        else exitFailure

confirm :: IO Bool
confirm = do
  putStrLn "This file alredy exists. Do you want to overwrite it? (y/n)"
  answer <- getLine
  case answer of
    "y" -> pure True
    "n" -> pure False
    _   -> putStrLn "Please use 'y' or 'n'" *> confirm

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
pInterpret = Interpret <$> pInput <*> pOutput

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
pParsing = Parse <$> pInput <*> pOutput

pTypeOf :: Parser Options
pTypeOf = TypeOf <$> pInput <*> pOutput

pREPL :: Parser Options
pREPL = do
  pure REPL

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
  <> header "SL - a simple language"
  <> progDesc "Command line for SL"
  )

optParse :: IO Options
optParse = Opt.execParser pOptsInfo

interpret :: FilePath -> Handle -> Handle -> IO ()
interpret file input output = do
  code <- hGetContents input
  ast <- execParser file (T.pack code)
  typ <- execTypeInfer ast
  val <- execEval ast
  hPutStrLn output $ "- : " ++ show typ ++ " = " ++ show val

parse :: FilePath -> Handle -> Handle -> IO ()
parse file input output = do
  code <- hGetContents input
  ast <- execParser file (T.pack code)
  hPrint output ast

typeof :: FilePath -> Handle -> Handle -> IO ()
typeof file input output = do
  code <- hGetContents input
  ast <- execParser file (T.pack code)
  typ <- execTypeInfer ast
  hPutStrLn output $ "- : " ++ show typ

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
