
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
  describe "Type Inference Test" $ do
    it "with AlgorithmW" $ do
      mapM (runExcept . runTestTypeInfer . fst) tiWithWPgms `shouldBe` Right (map snd tiWithWPgms)
    it "with AlgorithmM" $ do
      mapM (runExcept. runTestTypeInfer . fst) tiWithMPgms `shouldBe` Right (map snd tiWithMPgms)
  describe "Type Inference Error Test" $ do
    it "with AlgorithmW" $ do
      mapM (runExcept . runTestTypeInfer) tiWithWErrorPgms `shouldSatisfy` isLeft
    it "with AlgorithmM" $ do
      mapM (runExcept . runTestTypeInfer) tiWithMErrorPgms `shouldSatisfy` isLeft

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False
