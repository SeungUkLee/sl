module Program 
  ( interpretCliTestPgms
  , typeInferCliTestPgms
  , parsingCliTestPgms
  ) where

import           Interpreter
import           SLang

interpretCliTestPgms :: [TestCli()]
interpretCliTestPgms =
  [ mkInterpretCliPgm M Stdin Stdout
  , mkInterpretCliPgm M Stdin testInterpretOutputFile
  , mkInterpretCliPgm M testInterpretInputFile Stdout
  , mkInterpretCliPgm M testInterpretInputFile testInterpretOutputFile
  , mkInterpretCliPgm W Stdin Stdout
  , mkInterpretCliPgm W Stdin testInterpretOutputFile
  , mkInterpretCliPgm W testInterpretInputFile Stdout
  , mkInterpretCliPgm W testInterpretInputFile testInterpretOutputFile
  ]

parsingCliTestPgms :: [TestCli()]
parsingCliTestPgms =
  [ mkParsingCliPgm Stdin Stdout
  , mkParsingCliPgm Stdin testParseOutputFile
  , mkParsingCliPgm testParseInputFile Stdout
  , mkParsingCliPgm testParseInputFile testParseOutputFile
  ]

typeInferCliTestPgms :: [TestCli()]
typeInferCliTestPgms =
  [ mkTypeInferCliPgm M Stdin Stdout
  , mkTypeInferCliPgm M Stdin testTIOutputFile
  , mkTypeInferCliPgm M testTIInputFile Stdout
  , mkTypeInferCliPgm M testTIInputFile testTIOutputFile
  , mkTypeInferCliPgm W Stdin Stdout
  , mkTypeInferCliPgm W Stdin testTIOutputFile
  , mkTypeInferCliPgm W testTIInputFile Stdout
  , mkTypeInferCliPgm W testTIInputFile testTIOutputFile
  ]

testParseInputFile :: Input
testParseInputFile = InputFile "test/interative/files/hello.parse.sl"

testParseOutputFile :: Output
testParseOutputFile = OutputFile "test/interative/files/hello.parse.sl.out"

testTIInputFile :: Input
testTIInputFile = InputFile "test/interative/files/hello.typeinfer.sl"

testTIOutputFile :: Output
testTIOutputFile = OutputFile "test/interative/files/hello.typeinfer.sl.out"

testInterpretInputFile :: Input
testInterpretInputFile = InputFile "test/interative/files/hello.interpret.sl"

testInterpretOutputFile :: Output
testInterpretOutputFile = OutputFile "test/interative/files/hello.interpret.sl.out"
