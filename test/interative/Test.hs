{-# LANGUAGE OverloadedStrings #-}

import           Interpreter
import           Program
import           Test.Hspec
import           Test.Tasty
import           Test.Tasty.Hspec

main :: IO ()
main = do
  tree <- testSpec "Interative Test" spec
  defaultMain tree

spec :: Spec
spec = do
  describe "CLI" $ do
    it "parse test" $ do
      mapM runCli parsingCliTestPgms `shouldNotReturn` []

    it "typeinfer test" $ do
      mapM runCli typeInferCliTestPgms `shouldNotReturn` []

    it "interpret test" $ do
      mapM runCli interpretCliTestPgms `shouldNotReturn` []

  describe "CLI Error Test" $ do
    it "file does not exist" $ do
      mapM runCli errorCliTestPgms `shouldThrow` anyIOException
    it "parsing error" $ do
      mapM runCli errorParsingCliTestPgms `shouldThrow` anyException
    it "typeinfer error" $ do
      mapM runCli errorTypeInferCliTestPgms `shouldThrow` anyException
    it "interpret error" $ do
      mapM runCli errorInterpretCliTestPgms `shouldThrow` anyException
