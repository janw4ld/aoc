module Main (main) where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Function (on)
import Data.List (isPrefixOf, sortBy)

-- input = readFile "./test-input"
input = readFile "./input"
solve = sum . map (\l -> l !! (length l `div` 2)) . part2 . prepInput

part1 (ords, updates) = filter (isSafe ords) updates

part2 (ords, updates) =
  map ordify . filter (not . isSafe ords) $ updates
 where
  ordify = sortBy $ \a b -> if [a, b] `elem` ords then LT else GT

isSafe ords = all (`notElem` map reverse ords) . consAdjacent

prepInput =
  (bimap `on` map)
    (map (read @Int) . splitOn "|")
    (map (read @Int) . splitOn ",")
    . toTuple
    . splitOn [""]
    . lines

toTuple [a, b] = (a, b)
consAdjacent = zipAdjacentWith (\a b -> [a, b])
zipAdjacentWith f xs = zipWith f xs (drop 1 xs)
splitOn delim str
  | null str = [[]]
  | delim `isPrefixOf` str = [] : splitOn delim (drop (length delim) str)
  | otherwise =
      let (tl : rest) = splitOn delim (tail str)
       in (head str : tl) : rest

main = input >>= print . solve
