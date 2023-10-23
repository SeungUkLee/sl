{-# LANGUAGE ConstraintKinds #-}

module SLang.Program
  ( FinalSLang
  , Command (..)
  , Algorithm (..)
  , Interative (..)

  , mkInterpretPgm
  , mkTypeInferPgm

  , mkInterpretCliPgm
  , mkTypeInferCliPgm
  , mkParsingCliPgm
  , mkCliPgm
  )
where

import qualified Data.Text             as T
import           SLang.Eval
import           SLang.Interative.Cli
import           SLang.Parser
import           SLang.Pretty
import           SLang.TypeInfer       hiding (algorithmM, algorithmW)
import           SLang.TypeInfer.Class (SLangTypeInfer (algorithmM, algorithmW))

type FinalSLang m = (Interative m, Command m, Algorithm m)

class Interative m where
  cli :: (Pretty a) => (FilePath -> T.Text -> m a) -> m (InputHandle m) -> m (OutputHandle m) -> m ()
  repl :: m ()

  inputFile :: T.Text -> m (InputHandle m)
  outputFile :: T.Text -> m (OutputHandle m)

  stdin :: m (InputHandle m)
  stdout :: m (OutputHandle m)

class (SLangParser m, SLangTypeInfer m, SLangEval m) => Command m where
  interpret :: (Expr -> m Type) -> (FilePath -> T.Text -> m (Value, Type))

  typeinfer :: (Expr -> m Type) -> (FilePath -> T.Text -> m (Expr, Type))

  parsing :: FilePath -> T.Text -> m Expr

class (SLangTypeInfer m) => Algorithm m where
  w, m :: Expr -> m Type
  w = algorithmM
  m = algorithmW


{- | make SLang project program -}

mkInterpretPgm :: (Command m, Algorithm m) => TIAlgorithm -> FilePath -> T.Text -> m (Value, Type)
mkInterpretPgm algorithm = case algorithm of
  W -> interpret w
  M -> interpret m

mkTypeInferPgm :: (Command m, Algorithm m) => TIAlgorithm -> FilePath -> T.Text -> m (Expr, Type)
mkTypeInferPgm algorithm = case algorithm of
  W -> typeinfer w
  M -> typeinfer m

mkInterpretCliPgm :: (Command m, Interative m, Algorithm m) => TIAlgorithm -> Input -> Output -> m ()
mkInterpretCliPgm algorithm = mkCliPgm (mkInterpretPgm algorithm)

mkTypeInferCliPgm :: (Command m, Interative m, Algorithm m) => TIAlgorithm -> Input -> Output -> m ()
mkTypeInferCliPgm algorithm = mkCliPgm (mkTypeInferPgm algorithm)

mkParsingCliPgm :: (Command m, Interative m) => Input -> Output -> m ()
mkParsingCliPgm = mkCliPgm parsing

mkCliPgm :: (Interative m, Pretty a) => (FilePath -> T.Text -> m a) -> Input -> Output -> m ()
mkCliPgm cmd input output = case (input, output) of
  (Stdin, Stdout) -> cli cmd stdin stdout
  (Stdin, OutputFile file) -> cli cmd stdin (outputFile $ T.pack file)
  (InputFile file, Stdout) -> cli cmd (inputFile $ T.pack file) stdout
  (InputFile file, OutputFile file') -> cli cmd (inputFile $ T.pack file) (outputFile $ T.pack file')
