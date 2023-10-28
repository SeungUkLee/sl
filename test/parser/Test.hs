{-# LANGUAGE OverloadedStrings #-}

import           Test.Hspec
import           Test.Tasty
import           Test.Tasty.Hspec

import           Control.Monad.Except
import           Interpreter
import           Program

main :: IO ()
main = do
  tree <- testSpec "Interative Test" spec
  defaultMain tree


spec :: Spec
spec = do
  describe "Parser Test" $ do
    it "arthimetic (add, multiple) test" $ do
      mapM (runExcept . runTestParser . fst) parseArith `shouldBe` Right (map snd parseArith)
    it "if then else test" $ do
      mapM (runExcept . runTestParser . fst) parseIfThenElse `shouldBe` Right (map snd parseIfThenElse)
    it "lambda abstraction test" $ do
      mapM (runExcept . runTestParser . fst) parseFunc `shouldBe` Right (map snd parseFunc)
    it "let expression test" $ do
      mapM (runExcept . runTestParser . fst) parseLet `shouldBe` Right (map snd parseLet)
    it "let recursion test" $ do
      mapM (runExcept . runTestParser . fst) parseLetRec `shouldBe` Right (map snd parseLetRec)
    it "parse error test" $ do
      runExcept (runTestParser (parsePgm "let x = 1")) `shouldSatisfy` isLeft

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False
