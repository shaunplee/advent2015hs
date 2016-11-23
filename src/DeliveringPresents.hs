module DeliveringPresents where

-- map the ><^v directions to pairs of coordinates and sum them up with scanl, then count unique pairs

import           Data.Set as Set (fromList, size, union)

data Coord = Coord Integer Integer
    deriving (Show, Eq, Ord)

instance Monoid Coord where
    mappend (Coord xa xb) (Coord ya yb) = Coord (xa + ya) (xb + yb)
    mempty = Coord 0 0

dToCoords :: Char -> Coord
dToCoords '^' = Coord 0 1
dToCoords 'v' = Coord 0 (-1)
dToCoords '>' = Coord 1 0
dToCoords '<' = Coord (-1) 0
dToCoords _   = Coord 0 0

countHouses :: String -> Int
countHouses ds = Set.size $
    Set.fromList $ scanl mappend mempty $ map dToCoords ds

splitToPairs :: Monoid a => [a] -> [(a, a)]
splitToPairs []  = []
splitToPairs [ x ] = [ (x, mempty) ]
splitToPairs (x : y : xs) =
    (x, y) : splitToPairs xs

robotCountHouses :: String -> Int
robotCountHouses ds = let cs = map dToCoords ds
                          (santa, robot) = unzip $ splitToPairs cs
                          santaHouses = Set.fromList $
                              scanl mappend mempty santa
                          robotHouses = Set.fromList $
                              scanl mappend mempty robot
                      in
                          Set.size $ Set.union santaHouses robotHouses
