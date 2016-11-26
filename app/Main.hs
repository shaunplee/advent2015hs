module Main where

import           AdventCoins
import           AlmostLisp
import           DeliveringPresents
import           LiteBrite
import           NaughtyAndNice
import           System.Environment (getArgs)
import           Wrapping

main :: IO ()
main = do
    args <- getArgs
    content <- readFile (args !! 0)
    print $ LiteBrite.totalBrightness content
