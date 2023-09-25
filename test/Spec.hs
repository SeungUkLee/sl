{-# LANGUAGE LambdaCase #-}

import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Functor               ((<&>))

import           System.FilePath            (replaceExtension, takeBaseName)

import           Test.Tasty                 (TestTree, defaultMain, testGroup)
import           Test.Tasty.Golden          (findByExtension, goldenVsString)

import           Control.Exception          (Exception (displayException),
                                             Handler (Handler),
                                             SomeException (SomeException),
                                             catches)
import           Data.String
import qualified Data.Text.IO               as TIO
import           SLang                      (Result (Interpret),
                                             SLangError (..), execEval,
                                             execParser, execTypeInfer, pretty)

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
      ast <- execParser path script
      typ <- execTypeInfer ast
      val <- execEval ast
      return $ show $ pretty $ Interpret typ val
      )
      [ Handler $ \case
          ParseError e -> return $ displayException e
          TypeError e -> return $ displayException e
          EvalError e -> return $ displayException e
      , Handler $ \(SomeException e) -> return $ displayException e
      ]
