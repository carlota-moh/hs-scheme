module Evaluator
  ( eval
  ) where

import           Data (LispVal (..))

eval :: LispVal -> LispVal
-- match against a String and return the whole value
-- (not just what is inside the constructor)
eval val@(String _)             = val
eval val@(Number _)             = val
eval val@(Bool _)               = val
-- special case 
eval (List [Atom "quote", val]) = val
eval val                        = val
