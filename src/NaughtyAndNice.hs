module NaughtyAndNice where

import           Data.List (isInfixOf)

countNiceStrings :: String -> Int
countNiceStrings input = length $ filter partTwoNiceString (lines input)

-- Part 1
niceString :: String -> Bool
niceString s = threeVowels s && twiceInRow s && noBadSubstrings s

vowels = "aoeui"

isVowel :: Char -> Bool
isVowel = (`elem` vowels) -- The backticks and parentheses essentially
                          -- swap the arguments

threeVowels :: String -> Bool
threeVowels s = length (filter isVowel s) >= 3

twiceInRow :: String -> Bool
twiceInRow (x:xs) = let pairs = zip (x:xs) xs
                    in any (uncurry (==)) pairs
twiceInRow _ = False

noBadSubstrings :: String -> Bool
noBadSubstrings s = let badInfix = ["ab", "cd", "pq", "xy"]
                    in not $ or $ map (`isInfixOf` s) badInfix


-- Part 2
partTwoNiceString :: String -> Bool
partTwoNiceString s = pairAppearsTwice s && repeatWithLetterBetween s

pairAppearsTwice :: String -> Bool
pairAppearsTwice (x:y:xs) = if [x, y] `isInfixOf` xs
                            then True
                            else pairAppearsTwice (y:xs)
pairAppearsTwice _ = False

repeatWithLetterBetween :: String -> Bool
repeatWithLetterBetween (x:y:xs) = let pairs = zip (x:y:xs) xs
                                   in any (uncurry (==)) pairs
repeatWithLetterBetween _ =
    False
