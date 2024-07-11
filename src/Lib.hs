module Lib
    ( greet
    ) where
import System.Environment (getArgs)

greet :: IO ()
greet = do
    args <- getArgs
    putStrLn ("Hello " ++ head args)
