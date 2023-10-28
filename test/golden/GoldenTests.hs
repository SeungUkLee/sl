{-# LANGUAGE LambdaCase #-}

module GoldenTests
  ( goldenTests
  ) where

import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Functor               ((<&>))

import           System.FilePath            (replaceExtension, takeBaseName)

import           Test.Tasty                 (TestTree, testGroup)
import           Test.Tasty.Golden          (findByExtension, goldenVsString)

import           Control.Exception          (Exception (displayException),
                                             Handler (Handler),
                                             SomeException (SomeException),
                                             catches)
import           Data.String                (IsString (fromString))
import           Data.Text                  (Text)
import qualified Data.Text.IO               as TIO
import           Interpreter                (SLangGoldenTest (runSLangGoldenTest))
import           Program
import           SLang

goldenTests :: IO [TestTree]
goldenTests = sequence [goldenInterpretWithAlgorithmM , goldenInterpretWithAlgorithmW]

goldenInterpretWithAlgorithmW :: IO TestTree
goldenInterpretWithAlgorithmW = do
  paths <- listTestFiles
  w <- mapM algorithmWTest paths
  return $ testGroup "Interpret with AlgorithmW" w

goldenInterpretWithAlgorithmM :: IO TestTree
goldenInterpretWithAlgorithmM = do
  paths <- listTestFiles
  m <- mapM algorithmMTest paths
  return $ testGroup "Interpret with AlgorithmM" m

listTestFiles :: IO [FilePath]
listTestFiles = do
  findByExtension [".sl"] "test/golden/files"

type GoldenFileName = String

algorithmWTest :: FilePath -> IO TestTree
algorithmWTest = mkGoldenTests interpretWithW ".w.golden"

algorithmMTest :: FilePath -> IO TestTree
algorithmMTest = mkGoldenTests interpretWithM ".m.golden"

mkGoldenTests :: (Pretty a) => (FilePath -> Text -> SLangGoldenTest a) -> GoldenFileName -> FilePath -> IO TestTree
mkGoldenTests program golden path = do
  let testName = takeBaseName path
      goldenPath = replaceExtension path golden
  return (goldenVsString testName goldenPath $ action <&> BS.pack . fromString)
  where
    action = catches (do
      script <- TIO.readFile path
      res <- runSLangGoldenTest $ program path script
      return $ show $ pretty res
      )
      [ Handler $ \case
          ParserError e -> return $ displayException e
          TypeInferError e -> return $ displayException e
          EvaluatorError e -> return $ displayException e
      , Handler $ \(SomeException e) -> return $ displayException e
      ]
