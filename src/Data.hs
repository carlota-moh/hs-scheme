module Data
  ( LispVal(..)
  , primitives
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
  show (List contents)   = "(" ++ unwordsList contents ++ ")"
  show (DottedList h t)  = "(" ++ unwordsList h ++ " . " ++ show t ++ ")"

-- list of supported primitives, with a string key and a corresponding function
primitives :: [(String, [LispVal] -> LispVal)]
primitives =
  [ ("+", numericBinop (+))
  , ("-", numericBinop (-))
  , ("*", numericBinop (*))
  , ("/", numericBinop div)
  , ("mod", numericBinop mod)
  , ("quotient", numericBinop quot)
  , ("remainder", numericBinop rem)
  ]

-- Takes a function and a list of evaluated arguments, applies said function
-- and returns the result wrapped in the Number constructor
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params
  -- remove the constructor from the value to apply the operation
  where
    unpackNum (Number n) = n
    -- weak typing system means we can interpret "2" as a number
    unpackNum (String n) =
      let parsed = reads n :: [(Integer, String)]
       in if null parsed
            then 0
            else fst $ head parsed
    unpackNum (List [n]) = unpackNum n
    unpackNum _ = 0
