import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Functor               ((<&>))
import qualified Data.Text                  as T

import           System.FilePath            (replaceExtension, takeBaseName)

import           Test.Tasty                 (TestTree, defaultMain, testGroup)
import           Test.Tasty.Golden          (findByExtension, goldenVsString)

import           Eval                       (runEval)
import           Parser                     (parseSL)

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
  return (goldenVsString testName goldenPath $ action <&> BS.pack . show)
  where
    action = do
      script <- readFile path
      let p = parseSL $ T.pack script
      case p of
        -- TODO
        Left _  -> undefined
        Right e -> runEval e
