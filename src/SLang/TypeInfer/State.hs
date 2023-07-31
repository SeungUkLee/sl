module SLang.TypeInfer.State 
  ( InferState (count)
  , empty
  ) where

newtype InferState = InferState { count :: Int }
  deriving Show

empty :: InferState
empty = InferState { count = 0 }
