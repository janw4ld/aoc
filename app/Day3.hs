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
      (left, ',' : rest1) <- parseInt $ drop 4 str
      (right, ')' : rest2) <- parseInt rest1
      pure $ (left, right) : parse rest2
  | otherwise = parse $ tail str
 where
  parseInt = pure . first (read @Int) . span isDigit

splitOn delim str
  | null str = [[]]
  | delim `isPrefixOf` str = [] : splitOn delim (drop (length delim) str)
  | otherwise =
      let (h : rest) = splitOn delim (tail str)
       in (head str : h) : rest

main = input >>= print . solve
