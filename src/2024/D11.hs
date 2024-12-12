module Main (main) where

import Data.Bifunctor
import Data.Map qualified as M
import Data.Maybe

main = input >>= print . solve

-- input = pure "125 17"

input = pure "20 82084 1650 3 346355 363 7975858 0"

solve = snd . part2 . prepInput

part2 stones = blink cache 75 stones where (cache, _) = part1 stones

part1 = blink M.empty 25

blink cache _ [] = (cache, 0)
blink cache 0 stones = (cache, length stones)
blink cache n (stone : rest) | cached = second (l +) $ blink cache n rest
 where
  ml = cache M.!? (stone, n)
  cached = isJust ml
  l = fromJust ml
blink cache n (stone : rest) =
  second (l' +) $ blink (M.insert (stone, n) l' c') n rest
 where
  (c', l') = blink c1 (n - 1) s1
  (l1, s1) = updateStone stone
  c1 = M.insert (stone, 1) l1 cache

updateStone s
  | s == 0 = (1, [1])
  | even ls = (2, (\(a, b) -> map read [a, b]) . splitAt (ls `div` 2) $ show s)
  | otherwise = (1, [2024 * s])
 where
  ls = length . show $ s

prepInput = map (read @Int) . words
