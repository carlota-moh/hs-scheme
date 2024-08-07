module Parser
  ( runMyParser
  , parseString
  , parseAtom
  , parseNumber
  ) where

import           Text.ParserCombinators.Parsec hiding (spaces)

import           Control.Monad                 (liftM)
import           Data                          (LispVal (..))
import           System.Environment            (getArgs)

-- recognize any of the provided characters from input string
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

-- A string is a double quote mark, followed by any number of non-quote characters,
-- followed by a closing quote mark
parseString :: Parser LispVal
parseString = do
  -- discard '"', they just make sure this is a string
  _ <- char '"'
  x <- many (noneOf "\"")
  _ <- char '"'
  return $ String x

parseAtom :: Parser LispVal
parseAtom = do
  -- This tries the first parser, then if it fails, tries the second.
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return
    $ case atom of
       -- literal strings for true and false
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom atom

-- read converts the parsed string into an Integer, but because many1 digit returns a Parser String
-- (not a String), we use liftM to allow the composition, to work on the inside value
parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

-- parser that accepts either atom, string or number
parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber

-- run parser function (spaces + symbol) over input, using "lisp" for error messages
readExpr :: String -> String
readExpr input =
  case parse parseExpr "lisp" input of
    Left err  -> "No match: " ++ show err
    Right val -> "Found value! " ++ show val

runMyParser :: IO ()
runMyParser = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)
