{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}

import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Functor               ((<&>))

import           System.FilePath            (replaceExtension, takeBaseName)

import           Test.Tasty                 (TestTree, defaultMain, testGroup)
import           Test.Tasty.Golden          (findByExtension, goldenVsString)

import           Control.Exception          (Exception (displayException),
                                             Handler (Handler),
                                             SomeException (SomeException),
                                             catches)
import           Control.Monad.Catch        (MonadThrow)
import           Control.Monad.IO.Class     (MonadIO)
import           Data.String
import           Data.Text                  (Text)
import qualified Data.Text.IO               as TIO
import           SLang                      (Pretty, SLangError (..),
                                             SLangEval (..), SLangParser (..),
                                             SLangTypeInfer (..),
                                             TIAlgorithm (..), evaluate_,
                                             inferM_, inferW_, interpret,
                                             parse_, pretty)

newtype SLangTest a = SLangTest
  { runSLangTest :: IO a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadThrow
             )
instance SLangEval SLangTest where
  evaluate = evaluate_

instance SLangParser SLangTest where
  parse = parse_

instance SLangTypeInfer SLangTest where
  inferM = inferM_
  inferW = inferW_

main :: IO ()
main = do
  paths <- listTestFiles
  w <- mapM algorithmWTest paths
  m <- mapM algorithmMTest paths
  defaultMain
    ( testGroup "SLang Tests"
       [ testGroup "with AlgorithmM" w
       , testGroup "with AlgorithmW" m
       ]
    )

listTestFiles :: IO [FilePath]
listTestFiles = do
  findByExtension [".sl"] "test/Goldens"

type GoldenFileName = String

algorithmWTest :: FilePath -> IO TestTree
algorithmWTest = mkGoldenTests (interpret W) ".w.golden"

algorithmMTest :: FilePath -> IO TestTree
algorithmMTest = mkGoldenTests (interpret M) ".golden"

mkGoldenTests :: (Pretty a) => (FilePath -> Text -> SLangTest a) -> GoldenFileName -> FilePath -> IO TestTree
mkGoldenTests interpreter golden path = do
  let testName = takeBaseName path
      goldenPath = replaceExtension path golden
  return (goldenVsString testName goldenPath $ action <&> BS.pack . fromString)
  where
    action = catches (do
      script <- TIO.readFile path
      res <- runSLangTest $ interpreter path script
      return $ show $ pretty res
      )
      [ Handler $ \case
          ParserError e -> return $ displayException e
          TypeInferError e -> return $ displayException e
          EvaluatorError e -> return $ displayException e
      , Handler $ \(SomeException e) -> return $ displayException e
      ]
