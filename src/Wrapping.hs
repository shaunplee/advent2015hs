{-# LANGUAGE OverloadedStrings #-}

module Wrapping where

import           Data.List     (sort)
import           Text.Trifecta


parseWrapping :: String -> [Result (Integer, Integer, Integer)]
parseWrapping c = map (parseString parsePackage mempty) (lines c)

parsePackage :: Parser (Integer, Integer, Integer)
parsePackage = do
    l <- decimal
    char 'x'
    w <- decimal
    char 'x'
    h <- decimal
    return (l, w, h)

presentPaper :: Result (Integer, Integer, Integer) -> Integer
presentPaper (Success (l, w, h)) = let a = l*w
                                       b = w*h
                                       c = h*l
                                   in
                                       2*a + 2*b + 2*c + minimum [a, b, c]
presentPaper (Failure _) = 0

calculatePaper :: String -> Integer
calculatePaper c = sum $ map presentPaper $ parseWrapping c

presentRibbon :: Result (Integer, Integer, Integer) -> Integer
presentRibbon (Success (l, w, h)) =
    let [a, b, _] = sort [l, w, h]
    in 2 * a + 2 * b + l * w * h
presentRibbon (Failure _) = 0

calculateRibbon :: String -> Integer
calculateRibbon c = sum $ map presentRibbon $ parseWrapping c
