{- HLINT ignore "Use map once" -}
module Main (main) where

import Control.Monad
import Data.Bifunctor
import Data.Char
import Data.List (isPrefixOf)
import Data.Maybe

-- input = readFile "./test-input"
input = readFile "./input"
solve = part2 . prepInput

part1 = sum . map (uncurry (*)) . parse

part2 =
  sum
    . concatMap
      (map (uncurry (*)) . parse . head . splitOn "don't()")
    . splitOn "do()"

prepInput = id

parse str
  | str == "" = []
  | "mul(" `isPrefixOf` str = fromMaybe (parse (drop 4 str)) $ do
      let (left, c1 : rest1) = first (read @Int) $ span isDigit $ drop 4 str
      guard $ ',' == c1
      let (right, c2 : rest2) = first (read @Int) . span isDigit $ rest1
      guard $ ')' == c2
      pure $ (left, right) : parse rest2
  | otherwise = parse $ tail str

splitOn delim str
  | null str = [[]]
  | delim `isPrefixOf` str = [] : splitOn delim (drop (length delim) str)
  | otherwise =
      let (h : rest) = splitOn delim (tail str)
       in (head str : h) : rest

main = input >>= print . solve
