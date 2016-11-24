module NaughtyAndNice where

import           Data.List (isInfixOf)

vowels = "aoeui"

isVowel :: Char -> Bool
isVowel = (`elem` vowels) -- The backticks and parentheses essentially
                          -- swap the arguments

threeVowels :: String -> Bool
threeVowels s = length (filter isVowel s) >= 3

twiceInRow :: String -> Bool
twiceInRow []  = False
twiceInRow [_] = False
twiceInRow xs = let pairs = zip xs (tail xs)
    in any (uncurry (==)) pairs

noBadSubstrings :: String -> Bool
noBadSubstrings s = let badInfix = ["ab", "cd", "pq", "xy"]
    in not $ or $ map (`isInfixOf` s) badInfix

niceString :: String -> Bool
niceString s = threeVowels s && twiceInRow s && noBadSubstrings s

countNiceStrings :: String -> Int
countNiceStrings input = length $ filter niceString (lines input)
