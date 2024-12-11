{- HLINT ignore "Use map once" -}
module Main (main) where

import Control.Monad
import Data.Bifunctor
import Data.List

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
    . unzip
    . map (\[a, b] -> (a, b))
    . (map . map) read
    . map words
    . lines

main = input >>= print . solve
