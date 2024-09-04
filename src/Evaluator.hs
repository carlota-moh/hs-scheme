module Evaluator
  ( eval
  ) where

import           Data (LispVal (..), primitives)

-- apply a func to a list of evaluated args
apply :: String -> [LispVal] -> LispVal
-- lookup a key in a list of supported primitives.
-- If it fails, return Bool False, if it suceeds apply it to the list of args
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

eval :: LispVal -> LispVal
-- match against a String and return the whole value
-- (not just what is inside the constructor)
eval val@(String _)             = val
eval val@(Number _)             = val
eval val@(Bool _)               = val
-- special case
eval (List [Atom "quote", val]) = val
-- applying functions to args (ex: (+ 2 2) = 4)
eval (List (Atom func:args))    = apply func $ map eval args
eval val                        = val
