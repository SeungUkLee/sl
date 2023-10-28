{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.State
import qualified Data.Text                     as T
import           Interpreter
import           Program
import           SLang.Interative.Repl.Message (finalMessage, helpMessage,
                                                introMessage)
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

  describe "REPL Test" $ do
    it "help, quit command test" $ do
      res <- runTestRepl $ replTestPgm [ ":help" , ":quit" ]

      reverse res `shouldBe`
        [ T.unpack introMessage
        , T.unpack helpMessage
        , T.unpack finalMessage
        ]

    it "parse, type command test" $ do
      res <- runTestRepl $ replTestPgm
        [ ":parse let x = 1 in x"
        , ":type let f = fun x -> x in (f 1) + (f 2)"
        , ":quit"
        ]

      reverse res `shouldBe`
        [ T.unpack introMessage
        , "ELet (LBVal \"x\" (EConst (CInt 1))) (EVar \"x\")\n"
        , "let f = fun x -> x in f 1 + f 2 : int\n"
        , T.unpack finalMessage
        ]

    it "repl process test" $ do
      res <- runTestRepl $ replTestPgm
        [ "let x = 1 in x"
        , "1 + 2"
        , "let rec fac n = if n == 0 then 1 else n * (fac (n - 1)) in fac 5"
        , ":quit"
        ]

      reverse res `shouldBe`
        [ T.unpack introMessage
        , "- : int = 1\n"
        , "- : int = 3\n"
        , "- : int = 120\n"
        , T.unpack finalMessage
        ]

  describe "REPL Error Test" $ do
    it "load command error test (not exist file)" $ do
      res <- runTestRepl $ replTestPgm [ ":load" , ":quit" ]

      reverse res `shouldBe`
        [ T.unpack introMessage
        , ": openFile: does not exist (No such file or directory)"
        , T.unpack finalMessage
        ]


    it "no such command error test" $ do
      res <- runTestRepl $ replTestPgm [ ":hello" , ":quit" ]

      reverse res `shouldBe`
        [ T.unpack introMessage
        , "no such command : hello\n"
        , T.unpack finalMessage
        ]

    it "empty command error test" $ do
      res <- runTestRepl $ replTestPgm [ ":" , ":quit" ]

      reverse res `shouldBe`
        [ T.unpack introMessage
        , "empty command : To use the repl command, put ':' at the beginning.\n"
        , T.unpack finalMessage
        ]

runTestRepl :: TestRepl a -> IO a
runTestRepl testRepl = flip evalStateT (TestReplState [] []) $ unTestRepl testRepl
