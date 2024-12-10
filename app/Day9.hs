module Main (main) where

import Data.Char
import Data.List (transpose)

main = input >>= print . solve

-- input = readFile "./test-input"
-- input = pure "12345"
input = readFile "./input"

solve = part1 . prepInput

part1 disk = sum . zipWith (*) [0 ..] . intertwingle fileBlocks $ disk
 where
  fileBlocks = reverse . filter ((/= (-1)) . snd) . zip [0 ..] $ disk

intertwingle = intertwingle' 0
intertwingle' n fs mazen -- we're actually fragmenting the disk :)
  | null mazen = []
  | idx - n < 0 = d
  | b == -1 = v : intertwingle' (n + 1) frest d'
  | otherwise = b : intertwingle' (n + 1) fs d
 where
  d' = deleteAt (idx - n - 1) d
  b = head mazen
  d = reverse . dropWhile (== (-1)) . reverse . tail $ mazen
  frest = tail fs
  (idx, v) = head fs

part2 = id

prepInput =
  concat
    . flip (zipWith replicate) ids
    . map digitToInt
    . filter (/= '\n')
 where
  ids :: [Int] = interleave [0 ..] (repeat (-1))

interleave a b = concat . transpose $ [a, b]

deleteAt n xs -- 60% of cpu time is spent here, i should use a deque instead :3
  | n >= length xs = error "stop it, get help"
  | otherwise = take n xs <> drop (n + 1) xs
