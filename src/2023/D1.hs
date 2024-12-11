module Main (main) where

import Control.Monad
import Data.Bifunctor
import Data.Char
import Data.Function
import Data.Functor
import Data.List
import Data.Maybe
import Data.Set qualified as S
import Debug.Trace

main = input >>= print . solve

-- input = readFile "./test-input"

input = readFile "./input"

solve = sum . map (\l -> read @Int [head l, last l]) . part1 . prepInput

part1 = map (filter isDigit)

part2 = map parseDigits

parseDigits [] = []
parseDigits str@(c : rest)
  | isDigit c = c : parseDigits rest
  | isJust dName = d : parseDigits (drop dNameLen str)
  | otherwise = parseDigits rest
 where
  (dNameLen, d) = first length . fromJust $ dName
  dName = find ((`isPrefixOf` str) . fst) digitNames
  digitNames =
    [ ("one", '1')
    , ("two", '2')
    , ("three", '3')
    , ("four", '4')
    , ("five", '5')
    , ("six", '6')
    , ("seven", '7')
    , ("eight", '8')
    , ("nine", '9')
    ]

prepInput = lines
