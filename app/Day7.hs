module Main (main) where

import Data.Bifunctor
import Data.Function
import Data.List (isPrefixOf)

-- input = readFile "./test-input"

input = readFile "./input"
solve = sum . map fst . part1 . prepInput

part1 = filter (isSolvable ops) where ops = [(+), (*)]

part2 = filter (isSolvable ops)
 where
  ops = [(+), (*), \a b -> read @Int $ ((<>) `on` show) a b]

isSolvable _ (expected, [x]) = x == expected
isSolvable ops (expected, a : b : rest) =
  any
    (isSolvable ops . (expected,) . (\op -> op a b : rest))
    ops

prepInput =
  map
    ( bimap
        (read @Int)
        (map (read @Int) . words)
        . toTuple
        . splitOn ": "
    )
    . lines

toTuple [a, b] = (a, b)
splitOn delim str
  | null str = [[]]
  | delim `isPrefixOf` str = [] : splitOn delim (drop (length delim) str)
  | otherwise =
      let (tl : rest) = splitOn delim (tail str)
       in (head str : tl) : rest

main = input >>= print . solve
