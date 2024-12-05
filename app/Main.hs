{- HLINT ignore "Use map once" -}
module Main (main) where

import Control.Monad
import Data.Bifunctor
import Data.Function
import Data.Functor
import Data.List
import Data.Maybe
import Debug.Trace

input = readFile "./test-input"

-- input = readFile "./input"
solve = part1 . prepInput

part1 = id

part2 = id

prepInput = id

zipAdjacentWith f xs = zipWith f xs (drop 1 xs)
markSignChanges = zipAdjacentWith (\x y -> x * y < 0)
diffEachTwo = zipAdjacentWith (-)
groupByParity = \case
  [] -> ([], [])
  (x : xs) -> let (odds, evens) = groupByParity xs in (x : evens, odds)

rotate n xs = take (length xs) $ drop n $ cycle xs
deleteAt xs n = take n xs <> drop (n + 1) xs
trace1 v = trace (show v) v
infixl 0 |>
v |> f = f v

main = input >>= print . solve
