{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use map once" #-}
module Main (main) where

import Control.Monad
import Data.Bifunctor
import Data.List
import Debug.Trace

-- input = pure "3   4\n4   3\n2   5\n1   3\n3   9\n3   3"

input = readFile "./input"
solve = part1 . prepInput

part1 =
  sum
    . uncurry (zipWith (\a b -> abs (a - b)))

part2 =
  sum
    . \(l, r) -> map (\v -> v * length (filter (== v) r)) l

prepInput =
  join bimap sort
    -- . bisectByParity
    . unzip
    . map (\[a, b] -> (a, b))
    . (map . map) read
    . map words
    . lines

bisectByParity = \case
  (x : xs) -> let (odds, evens) = bisectByParity xs in (x : evens, odds)
  [] -> ([], [])
trace1 v = trace (show v) v

input :: IO String
main :: IO ()
main = input >>= print . solve
