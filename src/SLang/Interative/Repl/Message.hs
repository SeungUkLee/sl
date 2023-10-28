{-# LANGUAGE OverloadedStrings #-}

module SLang.Interative.Repl.Message
  ( introMessage
  , helpMessage
  , finalMessage
  )
where

import qualified Data.Text as T

introMessage :: T.Text
introMessage = T.unlines
  [ ""
  , "   _____ _                         Welcome to SLang REPL!"
  , "  / ____| |"
  , " | (___ | |     __ _ _ __   __ _   Author:  Seunguk Lee"
  , "  \\___ \\| |    / _` | '_ \\ / _` |  GitHub:  https://github.com/seunguklee/slang"
  , "  ____) | |___| (_| | | | | (_| |  Issues:  https://github.com/seunguklee/issues"
  , " |_____/|______\\__,_|_| |_|\\__, |  About:   ML dialect language with let-ploymorphic type system"
  , "                            __/ |  License: MIT"
  , "                           |___/"
  , ""
  , "Type ':help' for available commands"
  ]

helpMessage :: T.Text
helpMessage = T.unlines
  [ "Commands available from the REPL\n"
  , "   :help                       display this list of commands"
  , "   :load <file>                load file and interpret"
  , "   :parse <expr>               parse <expr> and show the AST of <expr>"
  , "   :quit                       exit REPL"
  , "   :type <expr>                show the type of <expr>"
  ]

finalMessage :: T.Text
finalMessage = "GoodBye!\n"
