{- HLINT ignore "Use map once" -}
module Main (main) where

import Control.Monad
import Data.Bifunctor
import Data.List
import Debug.Trace

input = readFile "./test-input"

-- input = readFile "./input"
solve = part1 . prepInput

part1 = id

part2 = id

prepInput = id

splitOn delim str
  | null str = [[]]
  | delim `isPrefixOf` str = [] : splitOn delim (drop (length delim) str)
  | otherwise =
      let (h : rest) = splitOn delim (tail str)
       in (head str : h) : rest

zipAdjacentWith f xs = zipWith f xs (tail xs)
markSignChanges = zipAdjacentWith (\x y -> x * y < 0)
diffEachTwo = zipAdjacentWith (-)
groupByParity = \case
  [] -> ([], [])
  (x : xs) -> let (odds, evens) = groupByParity xs in (x : evens, odds)

deleteAt xs n = take n xs <> drop (n + 1) xs
trace1 v = trace (show v) v
infixl 0 |>
v |> f = f v

main = input >>= print . solve
