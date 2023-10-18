module SLang.Interative.Repl.Class
  ( SLangRepl (..)
  ) where

class SLangRepl m where
  repl :: m ()
