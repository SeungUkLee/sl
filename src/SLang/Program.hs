module SLang.Program
  ( mkInterpretPgm
  , mkTypeInferPgm

  , mkInterpretCliPgm
  , mkTypeInferCliPgm
  , mkParsingCliPgm
  , mkCliPgm
  )
where

import qualified Data.Text        as T
import           SLang.Eval
import           SLang.Interative
import           SLang.Parser
import           SLang.Pretty
import           SLang.TypeInfer


{- | make SLang project program -}

mkInterpretPgm :: (SLangParser m, SLangTypeInfer m, SLangEval m) => TIAlgorithm -> FilePath -> T.Text -> m Result
mkInterpretPgm algorithm = case algorithm of
  W -> interpret algorithmW
  M -> interpret algorithmM

mkTypeInferPgm :: (SLangParser m, SLangTypeInfer m) => TIAlgorithm -> FilePath -> T.Text -> m Result
mkTypeInferPgm algorithm = case algorithm of
  W -> typeinfer algorithmW
  M -> typeinfer algorithmM

mkInterpretCliPgm :: (SLangCli m, SLangParser m, SLangTypeInfer m, SLangEval m) => TIAlgorithm -> Input -> Output -> m ()
mkInterpretCliPgm algorithm = mkCliPgm (mkInterpretPgm algorithm)

mkTypeInferCliPgm :: (SLangCli m, SLangParser m, SLangTypeInfer m) => TIAlgorithm -> Input -> Output -> m ()
mkTypeInferCliPgm algorithm = mkCliPgm (mkTypeInferPgm algorithm)

mkParsingCliPgm :: (SLangCli m, SLangParser m) => Input -> Output -> m ()
mkParsingCliPgm = mkCliPgm parsing

mkCliPgm :: (SLangCli m, Pretty a) => (FilePath -> T.Text -> m a) -> Input -> Output -> m ()
mkCliPgm cmd input output = case (input, output) of
  (Stdin, Stdout) -> cli cmd stdin stdout
  (Stdin, OutputFile file) -> cli cmd stdin (outputFile $ T.pack file)
  (InputFile file, Stdout) -> cli cmd (inputFile $ T.pack file) stdout
  (InputFile file, OutputFile file') -> cli cmd (inputFile $ T.pack file) (outputFile $ T.pack file')
