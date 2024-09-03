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
  deriving (Eq)

-- utility for applying unwords, which joins words separated by spaces
unwordsList :: [LispVal] -> String
unwordsList = unwords . map show

-- custom printing 
instance Show LispVal where
  show (String contents) = "\"" ++ contents ++ "\""
  show (Atom name)       = name
  show (Number contents) = show contents
  show (Bool True)       = "#t"
  show (Bool False)      = "#f"
  show (List contents) = "(" ++ unwordsList contents ++ ")"
  show (DottedList h t) = "(" ++ unwordsList h ++ " . " ++ show t ++ ")"
