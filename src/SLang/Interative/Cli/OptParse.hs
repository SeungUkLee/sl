{-# LANGUAGE OverloadedStrings #-}

module SLang.Interative.Cli.OptParse
  ( Input (..)
  , Output (..)
  , Options (..)

  , optParse
  )
where

import           Data.Maybe          (fromMaybe)
import           Options.Applicative (CommandFields, Mod, Parser, ParserInfo,
                                      command, execParser, fullDesc, header,
                                      help, helper, info, long, metavar,
                                      optional, progDesc, short, strOption,
                                      subparser, (<|>))

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

optParse :: IO Options
optParse = execParser pOptsInfo

pOptsInfo :: ParserInfo Options
pOptsInfo = info
  (helper <*> pOptions <|> pREPL)
  (fullDesc
  <> header "SLang - a simple language"
  <> progDesc "Command line for SLang"
  )

pOptions :: Parser Options
pOptions = subparser $ pInterpretCommand <> pParsingCommand <> pTypeOfCommand <> pREPLCommand

pInterpretCommand :: Mod CommandFields Options
pInterpretCommand = command "interpret" pInterpretInfo

pParsingCommand :: Mod CommandFields Options
pParsingCommand = command "parse" pParsingInfo

pTypeOfCommand :: Mod CommandFields Options
pTypeOfCommand = command "type" pTypeOfInfo

pREPLCommand :: Mod CommandFields Options
pREPLCommand = command "repl" pREPLInfo

pInterpretInfo :: ParserInfo Options
pInterpretInfo = info
  (helper <*> pInterpret)
  (progDesc "Interpret a SLang file")
  where
    pInterpret = InterpretOpt <$> pInput <*> pOutput

pParsingInfo :: ParserInfo Options
pParsingInfo = info
  (helper <*> pParsing)
  (progDesc "Parse a SLang file")
  where
    pParsing = ParseOpt <$> pInput <*> pOutput

pTypeOfInfo :: ParserInfo Options
pTypeOfInfo = info
  (helper <*> pTypeOf)
  (progDesc "Type Inference a SLang file")
  where
    pTypeOf = TypeOfOpt <$> pInput <*> pOutput

pREPLInfo :: ParserInfo Options
pREPLInfo = info
  (helper <*> pREPL)
  (progDesc "Enter a REPL for SLang")

pREPL :: Parser Options
pREPL = do
  pure REPLOpt

pInput :: Parser Input
pInput = fromMaybe Stdin <$> optional pInputFile
  where
    pInputFile = fmap InputFile parser
    parser = strOption
      ( long "input"
      <> short 'i'
      <> metavar "FILE"
      <> help "Input file"
      )

pOutput :: Parser Output
pOutput = fromMaybe Stdout <$> optional pOutputFile
  where
    pOutputFile = fmap OutputFile parser
    parser = strOption
      ( long "output"
      <> short 'o'
      <> metavar "FILE"
      <> help "Output file"
      )
