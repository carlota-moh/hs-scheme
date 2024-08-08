module Data
  ( LispVal(..)
  ) where

-- ADT for any possible value
data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool
  deriving (Show, Eq)
