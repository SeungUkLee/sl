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
import qualified Data.Text.IO               as TIO
import           SLang                      (SLangError (..), SLangEval (..),
                                             SLangParser (..),
                                             SLangTypeInfer (..), evaluate_,
                                             infer_, interpret, parse_, pretty)

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
  infer = infer_

main :: IO ()
main = do
  paths <- listTestFiles
  goldens <- mapM mkGoldenTest paths
  defaultMain (testGroup "Tests" goldens)

listTestFiles :: IO [FilePath]
listTestFiles = do
  findByExtension [".sl"] "test/Goldens"

mkGoldenTest :: FilePath -> IO TestTree
mkGoldenTest path = do
  let testName = takeBaseName path
  let goldenPath = replaceExtension path ".golden"
  return (goldenVsString testName goldenPath $ action <&> BS.pack . fromString)
  where
    action = catches (do
      script <- TIO.readFile path
      res <- runSLangTest $ interpret path script
      return $ show $ pretty res
      )
      [ Handler $ \case
          ParserError e -> return $ displayException e
          TypeInferError e -> return $ displayException e
          EvaluatorError e -> return $ displayException e
      , Handler $ \(SomeException e) -> return $ displayException e
      ]
