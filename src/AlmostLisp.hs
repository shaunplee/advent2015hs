module AlmostLisp where

almostLisp :: String -> Integer
almostLisp xs = sum (map cnt xs)

almostLispNeg :: String -> Integer
almostLispNeg xs = let indexed = zip [0..] (scanl (+) 0 (map cnt xs))
                       firstNeg = head $ dropWhile (\(_, v) -> v >= 0) indexed
                   in
                       fst firstNeg


cnt :: Char -> Integer
cnt '(' = 1
cnt ')' = -1
cnt _ = 0
