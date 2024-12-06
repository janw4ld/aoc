module Main (main) where

import Control.Monad
import Data.Bifunctor
import Data.Function
import Data.List (elemIndex, foldl', isPrefixOf)
import Data.Maybe

-- input = readFile "./test-input"
input = readFile "./input"
solve = sum . map (\l -> l !! (length l `div` 2)) . part2 . prepInput

part1 (ords, updates) = filter isSafe updates
 where
  unsafe = map reverse ords
  isSafe = all (`notElem` unsafe) . consAdjacent

part2 (ords, updates) =
  map (until isSafe ordify) . filter (not . isSafe) $ updates
 where
  unsafe = map reverse ords
  isSafe = all (`notElem` unsafe) . consAdjacent
  ordify u' =
    foldl'
      ( \u [l, r] -> fromMaybe u $ do
          lo <- elemIndex l u
          hi <- elemIndex r u
          guard $ lo > hi
          pure $ swap lo hi u
      )
      u'
      ords

prepInput =
  (bimap `on` map)
    (map (read @Int) . splitOn "|")
    (map (read @Int) . splitOn ",")
    . toTuple
    . map lines
    . splitOn "\n\n"

toTuple [a, b] = (a, b)
consAdjacent = zipAdjacentWith (\a b -> [a, b])
zipAdjacentWith f xs = zipWith f xs (drop 1 xs)
splitOn delim str
  | null str = [[]]
  | delim `isPrefixOf` str = [] : splitOn delim (drop (length delim) str)
  | otherwise =
      let (tl : rest) = splitOn delim (tail str)
       in (head str : tl) : rest
swap i' j' xs =
  if i' == j' then xs else left ++ [xs !! j] ++ middle ++ [xs !! i] ++ right
 where
  (i, j) = if i' > j' then (j', i') else (i', j')
  left = take i xs
  middle = take (j - i - 1) (drop (i + 1) xs)
  right = drop (j + 1) xs

main = input >>= print . solve
