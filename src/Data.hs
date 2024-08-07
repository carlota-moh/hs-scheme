module Data
  ( LispVal(..)
  ) where

-- ADT for any possible value
data LispVal
  = Atom String
  | List [LispVal]
  | Number Integer
  | String String
  | Bool Bool
  deriving (Show, Eq)
