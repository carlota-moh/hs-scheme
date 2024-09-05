module Data
  ( LispVal(..)
  , LispError(..)
  , ThrowsError
  , extractValue
  , trapError
  , primitives
  ) where

import           Control.Monad.Except
import           Text.Parsec          (ParseError)

-- ADT for any possible value
data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool
  deriving (Eq)

data LispError
  = NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | Parser ParseError
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | Default String

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

instance Show LispError where
  show (UnboundVar message varname) = message ++ ":  " ++ varname
  show (BadSpecialForm message form) = message ++ ": " ++ show form
  show (NotFunction message func) = message ++ ": " ++ show func
  show (NumArgs expected found) =
    "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
  show (TypeMismatch expected found) =
    "Invalid type: expected " ++ expected ++ ", found " ++ show found
  show (Parser parseErr) = "Parse error at " ++ show parseErr
  show (Default message) = message

-- partially applied type for functions that may fail and throw a LispError
type ThrowsError = Either LispError

-- catchError applies the func composition to the Either action if it is an error
-- so in practice we use this to transform one error into another
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

-- list of supported primitives, with a string key and a corresponding function
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
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
numericBinop ::
     (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _ [] = throwError $ NumArgs 2 []
numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op
-- numericBinop op params = Number $ foldl1 op $ map unpackNum params
  -- remove the constructor from the value to apply the operation
  where
    unpackNum :: LispVal -> ThrowsError Integer
    unpackNum (Number n) = return n
    -- weak typing system means we can interpret "2" as a number
    unpackNum (String n) =
      let parsed = reads n :: [(Integer, String)]
       in if null parsed
            then throwError $ TypeMismatch "number" $ String n
            else return (fst $ head parsed)
    unpackNum (List [n]) = unpackNum n
    unpackNum notNum = throwError $ TypeMismatch "number" notNum
