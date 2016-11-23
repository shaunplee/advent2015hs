Advent of Code 2015 in Haskell
==============================

I'm learning Haskell, so I thought I'd redo the [2015 AoC](http://adventofcode.com/2015) puzzles to prepare for the [2016](http://adventofcode.com/2016) puzzles and to get up to speed with the more practical aspects of solving code challenges in this language.

# [Day 1: Not Quite Lisp](http://adventofcode.com/2015/day/1)
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
