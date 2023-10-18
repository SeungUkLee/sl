
import           GoldenTests (goldenTests)
import           Test.Tasty  (defaultMain, testGroup)

main :: IO ()
main = do
  goldens <- goldenTests
  defaultMain
    ( testGroup "SLang Golden Tests" goldens
    )
