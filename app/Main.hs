module Main where

import           AlmostLisp
import           Lib
import           System.Environment (getArgs)
import           Wrapping

main :: IO ()
main = do
    args <- getArgs
    content <- readFile (args !! 0)
    print $ Wrapping.calculateRibbon content
