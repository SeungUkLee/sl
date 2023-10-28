{-# LANGUAGE OverloadedStrings #-}

module SLang.Interative.Repl.Error 
  ( ReplError (..)
  ) where

import           Control.Exception
import qualified Data.Text         as T

data ReplError
  = EmptyCommand T.Text
  | NoSuchCommand T.Text
  deriving (Show, Eq)

instance Exception ReplError where
  displayException = T.unpack . displayReplError

displayReplError :: ReplError -> T.Text
displayReplError (EmptyCommand prefix) = T.concat
  [ "empty command : "
  , "To use the repl command, put '", prefix, "'"
  , " at the beginning.", "\n"]
displayReplError (NoSuchCommand cmd) = T.concat
  [ "no such command : "
  , cmd, "\n"
  ]
