module Program
  ( interpretCliTestPgms
  , typeInferCliTestPgms
  , parsingCliTestPgms
  , errorCliTestPgms
  , errorInterpretCliTestPgms
  , errorParsingCliTestPgms
  , errorTypeInferCliTestPgms
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

errorCliTestPgms :: [TestCli()]
errorCliTestPgms =
  [ mkParsingCliPgm noExistFile Stdout
  , mkInterpretCliPgm M noExistFile Stdout
  , mkInterpretCliPgm W noExistFile Stdout
  , mkTypeInferCliPgm M noExistFile Stdout
  , mkTypeInferCliPgm W noExistFile Stdout
  ]

errorParsingCliTestPgms :: [TestCli()]
errorParsingCliTestPgms =
  [ mkParsingCliPgm errorTestParseInputFile Stdout
  ]

errorInterpretCliTestPgms :: [TestCli()]
errorInterpretCliTestPgms =
  [ mkInterpretCliPgm W errorTestInterpretInputFile Stdout
  , mkInterpretCliPgm M errorTestInterpretInputFile Stdout
  ]

errorTypeInferCliTestPgms :: [TestCli()]
errorTypeInferCliTestPgms =
  [ mkTypeInferCliPgm W errorTestTIInputFile Stdout
  , mkTypeInferCliPgm M errorTestTIInputFile Stdout
  ]

noExistFile :: Input
noExistFile = InputFile "test/interative/files/doesnotexist.sl"

errorTestParseInputFile :: Input
errorTestParseInputFile = InputFile "test/interative/files/error.parse.sl"

errorTestTIInputFile :: Input
errorTestTIInputFile = InputFile "test/interative/files/error.typeinfer.sl"

errorTestInterpretInputFile :: Input
errorTestInterpretInputFile = InputFile "test/interative/files/error.interpret.sl"

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
