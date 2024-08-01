module SimpleParser
  ( runSimpleParser
  ) where

import           Text.ParserCombinators.Parsec hiding (spaces)

import           Lib                           (getTwoFromList, pairToList)
import           System.Environment            (getArgs)

-- recognize any of the provided characters from input string
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

-- skip all spaces when parsing
spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
-- run parser function (spaces + symbol) over input, using "lisp" for error messages
readExpr input =
  -- attempt to match spaces, then symbol, fail if either fails
  case parse (spaces >> symbol) "lisp" input of
    Left err  -> "No match: " ++ show err
    Right val -> "Found value! " ++ show val

runSimpleParser :: IO ()
runSimpleParser =
  do
    listOfArgs <- getArgs
    let mbTwoElements = getTwoFromList listOfArgs
    case mbTwoElements of
      Nothing -> putStrLn "Hey, give me something to work with!"
      -- use mapM_ as the monadic version of map that does not collect list results
      Just strTuple -> mapM_ (putStrLn . readExpr) (pairToList strTuple)
        