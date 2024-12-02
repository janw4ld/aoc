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

markSignChanges (x : y : xs) = (x * y < 0) : markSignChanges (y : xs)
markSignChanges _ = []
diffEachTwo (x : y : xs) = x - y : diffEachTwo (y : xs)
diffEachTwo _ = []
bisectByParity = \case
  [] -> ([], [])
  (x : xs) -> let (odds, evens) = bisectByParity xs in (x : evens, odds)

deleteAt xs n = take n xs <> drop (n + 1) xs
trace1 v = trace (show v) v
infixl 0 |>
v |> f = f v

main = input >>= print . solve
