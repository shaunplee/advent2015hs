module Main where

import           AlmostLisp
import           DeliveringPresents
import           Lib
import           System.Environment (getArgs)
import           Wrapping

main :: IO ()
main = do
    args <- getArgs
    content <- readFile (args !! 0)
    print $ DeliveringPresents.robotCountHouses content
