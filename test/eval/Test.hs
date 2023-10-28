{-# LANGUAGE OverloadedStrings #-}

import           Test.Hspec
import           Test.Tasty
import           Test.Tasty.Hspec

import           Control.Monad.Except
import           Interpreter
import           Program

main :: IO ()
main = do
  tree <- testSpec "Eval Test" spec
  defaultMain tree

spec :: Spec
spec = do
  it "evaluate test" $ do
    mapM (runExcept . runTestEval . fst) evalPgms `shouldBe` Right (map snd evalPgms)
