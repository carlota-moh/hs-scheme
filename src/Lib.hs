module Lib
  ( greet
  , getTwoFromList
  , pairToList
  ) where

import           System.Environment (getArgs)

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

getTwoFromList :: [a] -> Maybe (a, a)
getTwoFromList []     = Nothing
getTwoFromList [_]    = Nothing
getTwoFromList (x:xs) = sequence (x, safeHead xs)

pairToList :: (a, a) -> [a]
pairToList (x, y) = [x, y]

greet :: IO ()
greet = do
  listOfArgs <- getArgs
  let mbTwoElements = getTwoFromList listOfArgs
  case mbTwoElements of
    Nothing       -> putStrLn "Hey, give me something to work with!"
    Just (a1, a2) -> putStrLn ("Hello " ++ a1 ++ a2)
