module Evaluator
  ( eval
  ) where

import           Control.Monad.Except
import           Data                 (LispError (..), LispVal (..),
                                       ThrowsError, primitives)

-- apply a func to a list of evaluated args
apply :: String -> [LispVal] -> ThrowsError LispVal
-- lookup a key in a list of supported primitives.
-- If it fails, return Bool False, if it suceeds apply it to the list of args
apply func args =
  maybe
    (throwError $ NotFunction "Unrecognized primitive function args" func)
    ($ args)
    (lookup func primitives)

eval :: LispVal -> ThrowsError LispVal
-- match against a String and return the whole value
-- (not just what is inside the constructor)
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
-- special case
eval (List [Atom "quote", val]) = return val
-- applying functions to args (ex: (+ 2 2) = 4)
eval (List (Atom func:args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm
