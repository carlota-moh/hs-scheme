module Parser
  ( runSimpleParser
  ) where

import           Text.ParserCombinators.Parsec hiding (spaces)

import           Lib                           (getTwoFromList, pairToList)
import           System.Environment            (getArgs)

-- recognize any of the provided characters from input string
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
-- run parser function (symbol) over input, using "lisp" for error messages
readExpr input =
  case parse symbol "lisp" input of
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
        