module Main (main) where

import Control.Monad
import Data.Char
import Data.List
import Data.Map.Strict qualified as M

main = input >>= print . solve

-- input = readFile "./test-input"

input = readFile "./input"

solve = part2 . prepInput

part2 = sum . map (M.foldl' (*) 1 . foldr1 @[] (unionWith max))

part1 =
  foldl'
    ( \acc -> \case
        (gno, m)
          | m M.! "red" <= 12 && m M.! "green" <= 13 && m M.! "blue" <= 14 ->
              gno + acc
          | otherwise -> acc
    )
    0
    . zip @Int [1 ..]
    . map (foldr1 @[] (unionWith max))

unionWith = M.unionWith @String @Int

prepInput =
  map
    ( map
        ( M.fromList
            . map ((\[v, c] -> (c, read @Int v)) . words)
            . splitOn ", "
        )
        . splitOn "; "
        . last
        . splitOn ": "
    )
    . lines

splitOn delim str
  | null str = [[]]
  | delim `isPrefixOf` str = [] : splitOn delim (drop (length delim) str)
  | otherwise =
      let (tl : rest) = splitOn delim (tail str)
       in (head str : tl) : rest
