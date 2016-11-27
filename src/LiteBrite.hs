module LiteBrite where

import           Control.Applicative
import qualified Data.Vector         as V
import           Text.Trifecta

-- Part 1

data Coord = Coord Int Int
    deriving Show

data Mode = On | Off | Toggle | Noop | AddOne | AddTwo | SubOne
    deriving Show

data Instruction = Instruction Mode Coord Coord
    deriving Show

newtype LightState = LightState (V.Vector (V.Vector Bool))
    deriving Show

countLightsOn :: String -> Int
countLightsOn input =
    let LightState finalPattern = patternLights (parseInput input)
        rowCounts = V.map (V.length . V.filter id) finalPattern
    in sum rowCounts

parseInput :: String -> V.Vector Instruction
parseInput s = V.fromList $ cleanupInstructions $ map parseLine (lines s)

cleanupInstruction :: Result Instruction -> Instruction
cleanupInstruction (Failure _) =
    Instruction Noop (Coord (-1) (-1)) (Coord (-1) (-1))
cleanupInstruction (Success x) =
    x

cleanupInstructions :: [Result Instruction] -> [Instruction]
cleanupInstructions = map cleanupInstruction

parseLine :: String -> Result Instruction
parseLine = parseString parseInstruction mempty

parseInstruction :: Parser Instruction
parseInstruction = parseOn <|> parseOff <|> parseToggle

parseOn :: Parser Instruction
parseOn = do
    _ <- string "turn on "
    (ca, cb) <- parseCoords
    return (Instruction On ca cb)

parseOff :: Parser Instruction
parseOff = do
    _ <- string "turn off "
    (ca, cb) <- parseCoords
    return (Instruction Off ca cb)

parseToggle :: Parser Instruction
parseToggle = do
    _ <- string "toggle "
    (ca, cb) <- parseCoords
    return (Instruction Toggle ca cb)

parseCoords :: Parser (Coord, Coord)
parseCoords = do
    startx <- decimal
    _ <- char ','
    starty <- decimal
    _ <- string " through "
    endx <- decimal
    _ <- char ','
    endy <- decimal
    return ((Coord (fromInteger startx) (fromInteger starty)),
            (Coord (fromInteger endx) (fromInteger endy)))

initState :: LightState
initState = LightState $ V.generate 1000 (const $ V.generate 1000 $ const False)

patternLights :: V.Vector Instruction -> LightState
patternLights = V.foldl' execInst initState

execInst :: LightState -> Instruction -> LightState
execInst (LightState state) (Instruction m start end) =
    let f = case m of
            On     -> const True
            Off    -> const False
            Toggle -> not
            _      -> id
    in LightState $ applyInst state f start end

applyInst :: V.Vector (V.Vector a)
          -> (a -> a)
          -> Coord
          -> Coord
          -> V.Vector (V.Vector a)
applyInst state f (Coord x1 y1) (Coord x2 y2) =
    let updateRow i =
            let curRow = state V.! i
                colUpdate j = (j, f (curRow V.! j))
                colUpdateV = V.map colUpdate (V.enumFromN y1 ((y2 - y1) + 1))
            in
                (i, curRow `V.update` colUpdateV)
        rowUpdateV = V.map updateRow (V.enumFromN x1 ((x2 - x1) + 1))
    in
        state `V.update` rowUpdateV

-- Part 2
newtype LightBrightness = LightBrightness (V.Vector (V.Vector Int))
    deriving Show

totalBrightness :: String -> Int
totalBrightness input = let LightBrightness finalPattern =
                                patternNordicLights (parseNordicInput input)
                            rowBrightness = V.map sum finalPattern
                        in
                            sum rowBrightness

parseNordicInput :: String -> V.Vector Instruction
parseNordicInput s = V.fromList $
    cleanupInstructions $ map parseNordicLine (lines s)

parseNordicLine :: String -> Result Instruction
parseNordicLine = parseString parseNordicInstruction mempty

parseNordicInstruction :: Parser Instruction
parseNordicInstruction = parseAddOne <|> parseAddTwo <|> parseSubOne

parseAddOne :: Parser Instruction
parseAddOne = do
    _ <- string "turn on "
    (ca, cb) <- parseCoords
    return (Instruction AddOne ca cb)

parseSubOne :: Parser Instruction
parseSubOne = do
    _ <- string "turn off "
    (ca, cb) <- parseCoords
    return (Instruction SubOne ca cb)

parseAddTwo :: Parser Instruction
parseAddTwo = do
    _ <- string "toggle "
    (ca, cb) <- parseCoords
    return (Instruction AddTwo ca cb)

initNordicState :: LightBrightness
initNordicState = LightBrightness $
    V.generate 1000 (const $ V.generate 1000 $ const (0 :: Int))

patternNordicLights :: V.Vector Instruction -> LightBrightness
patternNordicLights = V.foldl' execNordicInst initNordicState

execNordicInst :: LightBrightness -> Instruction -> LightBrightness
execNordicInst (LightBrightness state) (Instruction m start end) =
    let f = case m of
            AddOne -> (+ 1)
            SubOne -> (\x -> if x == 0 then 0 else x - 1)
            AddTwo -> (+ 2)
            _      -> id
    in
        LightBrightness $ applyInst state f start end
