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
