{-# LANGUAGE LambdaCase #-}

import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Functor               ((<&>))
import qualified Data.Text                  as T

import           System.FilePath            (replaceExtension, takeBaseName)

import           Test.Tasty                 (TestTree, defaultMain, testGroup)
import           Test.Tasty.Golden          (findByExtension, goldenVsString)

import           Control.Exception          (Handler (Handler),
                                             SomeException (SomeException),
                                             catches)
import           Data.String
import           SLang                      (SLangError (..), execEval,
                                             execParser, execTypeInfer)

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
      script <- readFile path
      ast <- execParser path $ T.pack script
      typ <- execTypeInfer ast
      val <- execEval ast
      return $ "- : " ++ show typ ++ " = " ++ show val
      )
      [ Handler $ \case
          ParseError e -> return $ show e
          TypeError e -> return $ show e
          EvalError e -> return $ show e
      , Handler $ \(SomeException e) -> return $ show e
      ]
