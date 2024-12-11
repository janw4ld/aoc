{- HLINT ignore "Use map once" -}
module Main (main) where

import Data.List (elemIndices)
import Data.Set qualified as S

-- input = readFile "./test-input"
input = readFile "./input"
solve = part2 . prepInput

part1 (maxY, maxX, antennas) =
  S.size . S.unions . S.map (anti . snd) $ antennas
 where
  anti (a : b : rest) = antiPair a b <> anti (a : rest) <> anti (b : rest)
  anti _ = S.empty
  antiPair (y1, x1) (y2, x2) =
    S.fromList
      . filter (\(y, x) -> 0 <= y && y <= maxY && 0 <= x && x <= maxX)
      $ [(y2 + dy, x2 + dx), (y1 - dy, x1 - dx)]
   where
    (dy, dx) = (y2 - y1, x2 - x1)

part2 (maxY, maxX, antennas) =
  S.size . S.unions . S.map (anti . snd) $ antennas
 where
  anti (a : b : rest) = antiSet a b <> anti (a : rest) <> anti (b : rest)
  anti _ = S.empty
  antiSet (y1, x1) (y2, x2) =
    S.unions
      . map
        ( \op ->
            S.fromList
              . takeWhile (\(y, x) -> 0 <= y && y <= maxY && 0 <= x && x <= maxX)
              $ [(y2 `op` (n * dy), x2 `op` (n * dx)) | n <- [0 ..]]
        )
      $ [(-), (+)]
   where
    (dy, dx) = (y2 - y1, x2 - x1)

prepInput str =
  (maxY,maxX,)
    . S.map
      ( \char ->
          (char,)
            . concatMap (\(y, line) -> map (y,) . elemIndices char $ line)
            . zip [0 ..]
            $ grid
      )
    $ antennaChars
 where
  antennaChars = S.delete '\n' . S.delete '.' . S.fromList $ str
  grid = lines str
  maxY = length grid - 1
  maxX = (length . head) grid - 1

main = input >>= print . solve
