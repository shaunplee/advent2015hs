Advent of Code 2015 in Haskell
==============================

I'm learning Haskell, so why not redo the [2015 AoC](http://adventofcode.com/2015) puzzles in Haskell to prepare for the [2016](http://adventofcode.com/2016) puzzles?

# [Day 1: Not Quite Lisp](http://adventofcode.com/2015/day/1)
## Part 1
Problem: Given a string of `(` and `)' characters, start at `0`, add `1` for every `(` and subtract `1` for every `)` and output the result.

Reading this problem took me back to that summer between 7th and 8th grade, when I took my first CS class and we worked through a good chunk of [The Schemer's Guide](https://www.amazon.com/Schemers-Guide-Iain-Ferguson/dp/0962874523) and we used this technique to manually count our parentheses.  Good times.

This problem actually comes down to counting the number of `(` and the number of `)`, and subtracting the first from the second.  Let's just count `1` for every `(` and count `-` for every `)` with a function I'll call `cnt`:
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
Problem: Instead of finding the total sum, find the position where the running sum first goes negative.

Hmm... wasn't there some kind function like `fold` that gave you a list rather than a single final accumulated total?  Oh yeah, `scanl :: (b -> a -> b) -> b -> [a] -> [b]`.

Okay, assuming that works, how I can get to that first negative value using `dropWhile (>=0)`, but how would I know its position?  Oh, I have to zip it with a position indicator, (like python's `enumerate`?) to get my `indexed` positions, then I can do `dropWhile`, but have to check the second part of the zipped tuples.  Once I get to that first negative, I can just take the head of the remaining list and the position is going to be the first part of the tuple:
```haskell
almostLispNeg :: String -> Integer
almostLispNeg xs = let indexed = zip [0 ..] (scanl (+) 0 (map cnt xs))
                       firstNeg = head $ dropWhile (\(_, v) -> v >= 0) indexed
                   in
                       fst firstNeg
```
