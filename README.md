Advent of Code 2015 in Haskell
==============================

I'm learning Haskell, so I thought I'd redo the [2015 AoC](http://adventofcode.com/2015) puzzles to prepare for the [2016](http://adventofcode.com/2016) puzzles and to get up to speed with the more practical aspects of solving code challenges in this language.

# [Day 1: Not Quite Lisp](http://adventofcode.com/2015/day/1)
[AlmostLisp.hs](./src/AlmostLisp.hs)
## Part 1
*Problem:* Given a string of `(` and `)' characters, start at `0`, add `1` for every `(` and subtract `1` for every `)` and output the result.

(Reading this problem took me back to that summer between 7th and 8th grade, when I took my first CS class and we worked through a good chunk of [The Schemer's Guide](https://www.amazon.com/Schemers-Guide-Iain-Ferguson/dp/0962874523) and we used this technique to manually count our parentheses.  Good times.)

The plan is to count the number of `(` and the number of `)`, and subtracting the first from the second.  Let's just count `1` for every `(` and count `-` for every `)` with a function I'll call `cnt`:
```haskell
cnt :: Char -> Integer
cnt '(' = 1
cnt ')' = -1
cnt _ = 0

```
now we just map `cnt` over the items and `sum` them up:
```haskell
almostLisp :: String -> Integer
almostLisp xs = sum (map cnt xs)
```
## Part 2
*Problem:* Instead of finding the total sum, find the position where the running sum first goes negative.

Wasn't there some function like `fold` that gave you a list rather than a single accumulated total?  Oh yeah, `scanl :: (b -> a -> b) -> b -> [a] -> [b]`.
```
λ> scanl (+) 0 (map cnt "(()))(()")
[0,1,2,1,0,-1,0,1,0]
```
Cool.

Okay, I can get to that first negative value using `dropWhile (>=0)`:
```
λ> dropWhile (>=0) $ scanl (+) 0 (map cnt "(()))(()")
[-1,0,1,0]
```
but how would I get the position of that first negative value after dropping the front part of the list?  I'll have to first zip the list with a position indicator (like python's `enumerate`?) to get indexed positions, then I can do `dropWhile`.  The predicate for `dropWhile` will have to change to test the second ("value") part of the tuples.  Once I get to that first negative, I can just take the head of the remaining list and the position is going to be the first part of the tuple:
```haskell
almostLispNeg :: String -> Integer
almostLispNeg xs = let indexed = zip [0 ..] (scanl (+) 0 (map cnt xs))
                       firstNeg = head $ dropWhile (\(_, v) -> v >= 0) indexed
                   in
                       fst firstNeg
```

## Lessons learned
- `scanl`
- indexing lists with `zip`

# [Day 2: I Was Told There Would Be No Math](http://adventofcode.com/2015/day/2)
[Wrapping.hs](./src/Wrapping.hs)
## Part 1
*Problem:* Given a list of dimensions of rectangular prism presents (e.g., "2x3x4"), calculate the amount of wrapping paper needed for each present as the surface area plus the area of the smallest size.

In Python, I might have read the input with something like:
```python
with open(filename) as f:
    presents = [int(x) for x in line.split('x') for line in f]
    ...
```
So I went hoogle-ing for an `int` equivalent (`String -> Int`?) and a `split` equivalent (`String -> [String]`).

Nope.  Bad idea.  After an hour of trying to map `read` over a list of `String`s, I remembered that Haskell is known for its parsing libraries and dug back into the [book](http://haskellbook.com) I've been reading:
```haskell
import           Text.Trifecta

parsePackage :: Parser (Integer, Integer, Integer)
parsePackage = do
    l <- decimal
    char 'x'
    w <- decimal
    char 'x'
    h <- decimal
    return (l, w, h)
```

I don't understand everything going on here.  I do know that I'm parsing a `decimal` followed by an `x` followed by another `decimal`, another `x`, and one more `decimal`.  The function then returns the three parsed `Integers` in the `Parser` monad. (Psst. I can tell it's a monad because of the `return`.)

Ok, great.  I can get each present in a separate string in a list using `lines`, I can parse each package separately to get a list of tuples (Integer, Integer, Integer).

Oh, actually, the `Integer` tuples are still in the `Result` monad, so my attempt to parse the input will give me a list of `Result`s.  Let's try it:

```haskell
parseWrapping :: String -> [Result (Integer, Integer, Integer)]
parseWrapping c = map (parseString parsePackage mempty) (lines c)
```

How much paper does it take to wrap a present?  `2*l*h + 2*l*w + 2*w*h + smallest side`.  Recalling that we have list of `Result`s, We can do:
```haskell
presentPaper :: Result (Integer, Integer, Integer) -> Integer
presentPaper (Success (l, w, h)) =
    let a = l * w
        b = w * h
        c = h * l
    in
        2 * a + 2 * b + 2 * c + minimum [ a, b, c ]
```

Hmm, but now the compiler is complaining that the pattern matches are non-exhaustive.  I haven't covered the case where the argument is a `Failure`.  Well, if it's a `Failure`, let's say that the wrapping paper needed is `0`, because we didn't parse a valid present size:
```haskell
presentPaper (Failure _) = 0
```
(This is awesome.  In Python, I would have just written the happy path and ignored the possibility of failure of my `split("x")` approach.)

Now we can put it all together to calculate the total amount of wrapping paper needed.  `c` is the input from the file of present dimensions, separated by newlines, the Parser turns these into `Result (Integer, Integer, Integer)`, and we can map `presentPaper` over these and add up the results to get the total amount of paper needed:
```haskell
calculatePaper :: String -> Integer
calculatePaper c = sum $ map presentPaper $ parseWrapping c
```
## Part 2
*Problem:* Now calculate the ribbon needed, which is the perimeter of the smallest face plus the volume of the present (for the bow).

Nothing much here, just adapt the paper calculation to calculate the ribbon length instead.  The only difference is that we need the two smallest values rather than the minimum size.  To do that, let's use `sort` from `Data.List`:
```haskell
import           Data.List     (sort)

presentRibbon :: Result (Integer, Integer, Integer) -> Integer
presentRibbon (Success (l, w, h)) =
    let (a : b : _) = sort [ l, w, h ]
    in
        2 * a + 2 * b + l * w * h
presentRibbon (Failure _) = 0
```
And then we just run in the same way as with the paper:
```haskell
calculateRibbon :: String -> Integer
calculateRibbon c = sum $ map presentRibbon $ parseWrapping c
```
## Lessons learned
- Use parsers, they're not that bad.

# [Day 3: Perfectly Spherical Houses in a Vacuum](http://adventofcode.com/2015/day/3)
[DeliveringPresents.hs](./src/DeliveringPresents.hs)
## Part 1
*Problem:* Santa receives a list of directions (the chars `^v<>`) and delivers presents to an infinite 2D grid of houses. For each character read, Santa moves one house in that direction (e.g., `^` means move one house north).  How many unique houses will Santa visit?

The plan is to represent each location that Santa visits as a coordinate pair, e.g., `(0,0)` for the starting location.  Then each direction gets mapped to a direction (e.g., `^` becomes `(0,1)`, `<` becomes `(-1,0)`, etc.).  Finding each visited house for each move just becomes a `scanl`, because we're accumulating the steps.

Let's define a `Coord` data type to keep track of Santa's location. (There's probably some nicer way to use `newtype` or something to accomplish this...I should look in to that.)
```haskell
data Coord = Coord Integer Integer
    deriving (Show, Eq, Ord)
```
And come to think of it, what we're actually doing is sort of like a monoid, so let's define that as well:
```haskell
instance Monoid Coord where
    mappend (Coord xa xb) (Coord ya yb) = Coord (xa + ya) (xb + yb)
    mempty = Coord 0 0
```
Let's map those directions to `Coord`s that we can `<>` together:
```haskell
dToCoords :: Char -> Coord
dToCoords '^' = Coord 0 1
dToCoords 'v' = Coord 0 (-1)
dToCoords '>' = Coord 1 0
dToCoords '<' = Coord (-1) 0
dToCoords _   = Coord 0 0
```
To find the unique `Coord`s only, I'll dump them into a `Set`, then count the size of the `Set`, so we can put the pieces together like this:
```haskell
countHouses :: String -> Int
countHouses ds = Set.size $
    Set.fromList $ scanl mappend mempty $ map dToCoords ds
```
This maps our `dToCoords` function over the input list of directions `ds`.  We then use `scanl` with the conveniently defined `Monoid` instance of `Coords` to accumulate Santa's position with each move.  `Set.fromList` turns the list into a `Set`, and `Set.size` gives us the number of unique houses visited.

## Part 2
*Problem:* Now Santa creates a Robo-Santa to help deliver presents.  Robo-Santa and Santa start at the same location, but they take turns moving based on alternate instructions.  Now how many unique houses are visited?

We're going to split up the inputs with alternating values going to Santa or Robo-Santa, run the `scanl` stuff on Santa and Robo-Santa separately, then take the size of the union of the resulting sets.

There must be a better way to split up alternating values in a list.  Something like `deinterleave`.  Oh well:
```haskell
splitToPairs :: Monoid a => [a] -> [(a, a)]
splitToPairs []  = []
splitToPairs [ x ] = [ (x, mempty) ]
splitToPairs (x : y : xs) =
    (x, y) : splitToPairs xs
```
All this does is take adjacent values and put them into tuples.  Once we do that, we can `unzip` to split these into two separate lists (e.g., the lists `santa` and `robot`.
```haskell
robotCountHouses :: String -> Int
robotCountHouses ds = let cs = map dToCoords ds
                          (santa, robot) = unzip $ splitToPairs cs
                          santaHouses = Set.fromList $
                              scanl mappend mempty santa
                          robotHouses = Set.fromList $
                              scanl mappend mempty robot
                      in
                          Set.size $ Set.union santaHouses robotHouses
```
Then the rest goes according to plan.
## Lessons learned
- `scanl` is great
- `monoid`s are everywhere
- I probably didn't need a `monoid` in this case.
- How do you deal with the element pairing thing?

# [Day 4: The Ideal Stocking Stuffer](http://adventofcode.com/2015/day/4)
[AdventCoins.hs](./src/AdventCoins.hs)

*Problem:* Given a secret key (8 characters), find the smallest positive integer that you can concatenate onto the end of the secret key such that the MD5 hash of the concatenated value starts with {five, six} `0`s.

First, how do we calculate MD5 hashes in Haskell?  Hoogle suggests there's some sort of `Crypto.Hash` library.  Google says it comes from `cryptonite`.  The docs say there's:
```haskell
hashlazy :: HashAlgorithm a => ByteString -> Digest a
```
That seems as good as anything, whatever this `ByteString` thing is.  Oh, there's an `MD5` instance of the `HashAlgorithm` class.  That seems applicable.

This is my first time encountering `ByteString`, though I've heard that Haskell has this problem with ~3 different ways to express strings. Hoogle saves the day again, as searching for `String -> ByteString` gives this as one of the results:
```haskell
Data.ByteString.Lazy.UTF8 fromString :: String -> ByteString
```
Let's import this stuff:
```haskell
import           Crypto.Hash
import qualified Data.ByteString.Lazy      as LB
import           Data.ByteString.Lazy.UTF8 (fromString)
```
And let's define an `md5` hashing function:
```haskell
md5 :: LB.ByteString -> Digest MD5
md5 = hashlazy
```
And let's see how it works:
```haskell
λ> md5 (fromString "abc")
900150983cd24fb0d6963f7d28e17f72
```
Well, it gave me something back, but it's a `Digest MD5`, so I don't know how I'll be able to pattern match on it to find 5 leading `0`s.  On the other hand, it printed to the repl, so it must be an instance of `Show`.  Let's abuse this:
```haskell
λ> show $ md5 (fromString "abc")
"900150983cd24fb0d6963f7d28e17f72"
```
Now we can pattern match on it like it's a `String` (there's gotta be a better way).  The rest should be self explanatory:
```haskell
mine :: String -> Integer
mine secret = mineRecur (fromString secret) 1 where
    mineRecur sec c =
        let try = LB.append sec (fromString $ show c)
        in case show $ md5 try of
            ('0':'0':'0':'0':'0':_) -> c
            otherwise               -> mineRecur sec (c + 1)
```
## Lessons learned
- Haskell libraries are not all that scary!
- Compiling makes things run a little faster

# [Day 5: Doesn't He Have Intern-Elves For This?](http://adventofcode.com/2015/day/5)
[NaughtyAndNice.hs](./src/NaughtyAndNice.hs)
## Part 1
*Problem:*
> A nice string is one with all of the following properties:

> - It contains at least three vowels (aeiou only), like aei, xazegov, or aeiouaeiouaeiou.
> - It contains at least one letter that appears twice in a row, like xx, abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
> - It does not contain the strings ab, cd, pq, or xy, even if they are part of one of the other requirements.
Given a list of strings, count the number of "nice" strings.

Let's filter the input to keep only the nice strings, then count the length of the filtered list.
```haskell
countNiceStrings :: String -> Int
countNiceStrings input = length $ filter niceString (lines input)
```
`niceString` is just the conjunction of the three rules:
```haskell
niceString :: String -> Bool
niceString s = threeVowels s && twiceInRow s && noBadSubstrings s
```
So let's write the three rules.  First, we want to make sure that there are at least three vowels in the string
```haskell
vowels = "aoeui"

isVowel :: Char -> Bool
isVowel = (`elem` vowels) -- The backticks and parentheses essentially
                          -- swap the arguments

threeVowels :: String -> Bool
threeVowels s = length (filter isVowel s) >= 3
```
Next, we'll look for at least one occurrence of a character that appears twice in a row.  To do this, we'll zip the list with its `tail` (the `rest` or `cdr`) and then check to see if any of the pairs are the same `Char`. If the list is too short to pattern match, then it's an automatic `False`.
```haskell
twiceInRow :: String -> Bool
twiceInRow (x:xs) = let pairs = zip (x:xs) xs
                    in any (uncurry (==)) pairs
twiceInRow _ = False
```
HLint suggested the `uncurry (==)` thing in place of my original `(\(x, y) -> x == y`.  I like it.

Finally, we want to make sure there are no "bad strings":
```haskell
noBadSubstrings :: String -> Bool
noBadSubstrings s = let badInfix = ["ab", "cd", "pq", "xy"]
    in not $ or $ map (`isInfixOf` s) badInfix
```
## Part 2
*Problem:*
New rules:
> Now, a nice string is one with all of the following properties:

> - It contains a pair of any two letters that appears at least twice in the string without overlapping, like xyxy (xy) or aabcdefgaa (aa), but not like aaa (aa, but it overlaps).
> - It contains at least one letter which repeats with exactly one letter between them, like xyx, abcdefeghi (efe), or even aaa.

We'll edit our `countNiceStrings` to filter with `partTwoNiceString`:
```haskell
countNiceStrings :: String -> Int
countNiceStrings input = length $ filter partTwoNiceString (lines input)

partTwoNiceString :: String -> Bool
partTwoNiceString s = pairAppearsTwice s && repeatWithLetterBetween s
```
and now we can define those two properties.

For `pairAppearsTwice`, we'll take the first two characters in the list and see if they appear in the rest of the list. If so, then `True` and if not, then we'll recursively call `pairAppearsTwice` with the tail of the list. If there aren't enough characters in the list to pattern match, then we return `False`.
```haskell
pairAppearsTwice :: String -> Bool
pairAppearsTwice (x:y:xs) = if [x, y] `isInfixOf` xs
                            then True
                            else pairAppearsTwice (y:xs)
pairAppearsTwice _ = False
```
`repeatWithLetterBetween` is pretty much the same as `twiceInRow` but we now zip with the `tail` of the `tail` (`cddr`).
```haskell
repeatWithLetterBetween :: String -> Bool
repeatWithLetterBetween (x:y:xs) = let pairs = zip (x:y:xs) xs
                                   in any (uncurry (==)) pairs
repeatWithLetterBetween _ = False
```
## Lessons learned
- Haskell is more like Clojure than I originally gave it credit for.
- HLint is great for expanding your Haskell vocabulary.

# [Day 6: Probably a Fire Hazard](http://adventofcode.com/2015/day/6)
[LiteBrite.hs](./src/LiteBrite.hs)
## Part 1
*Problem:* Given a list of instructions for controlling a 1000x1000 grid of lights, how many lights are lit after executing all of the instructions?  Instructions have the form:
```
turn on nn,nn through nn,nn
turn off nn,nn through nn,nn
toggle nn,nn through nn,nn
```
where the `nn` are non-negative integers less than `1000`.

I'll eventually want to do something where I fold over the list of instructions, to update the state of the lights, then count the final number of lights that are on.

Let's set up some data types for handling this data.  Let's also learn to use `Data.Vector`.  The current state of the lights is just a 2D `Vector` of `Bool`s:

```haskell
import qualified Data.Vector         as V

newtype LightState = LightState (V.Vector (V.Vector Bool))
    deriving Show
```

I'll initialize the `LightState` using `Vector.generate`:

```haskell
initState :: LightState
initState = LightState $ V.generate 1000 (const $ V.generate 1000 $ const False)
```

I'll represent each instruction based on the command `turn on`, `turn off`, or `toggle`, which I'll call a `Mode` (I'm not so great at naming), as well as two coordinate pairs that identify the rectangles that the command should apply to:

```haskell
data Coord = Coord Int Int
    deriving Show

data Mode = On | Off | Toggle | Noop
    deriving Show

data Instruction = Instruction Mode Coord Coord
    deriving Show
```

I'm going to build up to something like this, where I parse the input to generate a list of instructions, then fold over the instructions (in `patternLights`) to generate a `finalPattern`.  I'll then count the number of `True` values in each row of the state using `V.length . V.filter id`.  This will gives a list of `Int`s (one for each row of the state), so I'll sum them up to get our total:

```haskell
countLightsOn :: String -> Int
countLightsOn input =
    let LightState finalPattern = patternLights (parseInput input)
        rowCounts = V.map (V.length . V.filter id) finalPattern
    in sum rowCounts
```

Let's write more parsers:

```haskell
import Text.Trifecta

parseOn :: Parser Instruction
parseOn = do
    _ <- string "turn on "  -- we dump the text, but still match on it
    (ca, cb) <- parseCoords
    return (Instruction On ca cb) -- and detecting the "turn on" is captured
                                  -- here by the On type

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
```

We'll do the same for the `turn off` and `toggle`:

```haskell
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
```

In order to match a line of the input with the correct one of these three, we'll use the `<|>` operator from `Control.Applicative`:

```haskell
import           Control.Applicative

parseInstruction :: Parser Instruction
parseInstruction = parseOn <|> parseOff <|> parseToggle
```

This way, if `parseOn` fails (because the line didn't start with "`turn on`", the parser will continue by trying `parseOff`, then `parseToggle` before giving up and returning `Failure`.  This is super cool.

Given this, the `parseInput` function from the beginning is pretty simple.  `cleanupInstructions` is used to `unwrap` the `Result`s from the parser, and I convert the `Failure`s into `Noop`s.  Presumably, this won't ever be needed.

```haskell
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
```

Ok, now all the machinery is in place to parse those input lines:

```haskell
λ> parseLine "turn on 0,0 through 300,350"
Success (Instruction On (Coord 0 0) (Coord 300 350))
λ>
λ>
λ> parseInput "turn on 0,0 through 300,350\ntoggle 42,42 through 100,110"
[Instruction On (Coord 0 0) (Coord 300 350),Instruction Toggle (Coord 42 42) (Coord 100 110)]
```

Cool.  A lot of work, but cool.

The aforementioned `patternLights` is just a fold over the instructions:

```haskell
patternLights :: V.Vector Instruction -> LightState
patternLights = V.foldl' execInst initState
```

(I'm assuming `foldl'` to be, by default, the right choice, though I haven't thought this through).

The magic then, is going to be this `execInst`, which takes a `LightState` and an `Instruction` to generate a new `LightState`:

```haskell
execInst :: LightState -> Instruction -> LightState
execInst (LightState state) (Instruction m start end) =
    let f = case m of
            On     -> const True
            Off    -> const False
            Toggle -> not
            _      -> id
    in LightState $ applyInst state f start end
```

I'll match on the `Mode` to identify the right function that we'll `map` onto the values in the rectangle defined by `start` and `end`, then delegate the hard work to `applyInst`.

```haskell
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
```

Yikes.  The idea here is that, to update a `Vector a`, `V.update` takes a `Vector` of `(Int, a)` tuples and returns a new `Vector a` with the values at the locations specified by the `car` of the tuple with the value at the `cadr` of the tuple.

Let's start with updating the `i`th row using `updateRow`.  We pull out that row using `state V.! i` and name it `curRow`.  If a `j`th value is to be updated in that row, it should be replaced with the function `f` applied to the previous value.  Therefore, `colUpdate j = (j, f (curRow V.! j))`, and we can generate the tuple pairs for the `update` by mapping `colUpdate` over an enumeration of the positions to be updated (the `y` coordinates).  The final tuple for that row is therefore `(i, curRow `V.update` colUpdateV)`.

Now to update the 2D vector, I use the newly defined `updateRow` and run this on all the rows that should be affected (the `x` coordinates).  Hence `rowUpdateV = V.map updateRow (V.enumFromN x1 ((x2 - x1) + 1))`, and then we apply the update.

That was probably a lot more machinery than I really needed to solve this problem.

## Part 2

Is not so interesting.  Just a rewrite to use `Int` instead of `Bool` and change the parsers to generate add or subtract `Mode`s instead.  the key difference is the new `execNordicInst` matches to functions for computing those additions and subtractions.  The core `applyInst` function works unchanged.
