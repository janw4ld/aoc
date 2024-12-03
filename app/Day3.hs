{- HLINT ignore "Use map once" -}
module Main (main) where

import Control.Monad
import Data.Bifunctor
import Data.Char
import Data.List
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

prepInput = ("do()" <>)

parse str
  | str == "" = []
  | "mul(" `isPrefixOf` str = fromMaybe (parse (drop 4 str)) $ do
      let (left, rest1) = first (read @Int) $ span isDigit $ drop 4 str
      guard $ validInt left && "," `isPrefixOf` rest1
      let (right, rest2) = first (read @Int) . span isDigit $ drop 1 rest1
      guard $ validInt right && ")" `isPrefixOf` rest2
      pure $ (left, right) : parse (drop 1 rest2)
  | otherwise = parse $ tail str
 where
  validInt x = x < 1000 && x >= 0

splitOn delim str
  | null str = [[]]
  | delim `isPrefixOf` str = [] : splitOn delim (drop (length delim) str)
  | otherwise =
      let (h : rest) = splitOn delim (tail str)
       in (head str : h) : rest

main = input >>= print . solve
