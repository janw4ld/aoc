module Main (main) where

import Debug.Trace

-- input = readFile "./test-input"

input = readFile "./input"
solve = part2 . prepInput

part2 =
  length
    . filter id
    . map
      ( \xs ->
          any
            (isSafe . diffEachTwo . (xs `deleteAt`))
            [0 .. length xs - 1]
      )

part1 =
  length
    . filter fst
    . trace1
    . map (\l -> (isSafe l, l))
    . map diffEachTwo

isSafe l = all not (markSignChanges l) && all inSafeRange l
inSafeRange x = abs x > 0 && abs x <= 3

markSignChanges (x : y : xs) = (x * y < 0) : markSignChanges (y : xs)
markSignChanges _ = []

diffEachTwo (x : y : xs) = x - y : diffEachTwo (y : xs)
diffEachTwo _ = []

prepInput =
  (map . map) (read @Int)
    . map words
    . lines

deleteAt xs n = take n xs <> drop (n + 1) xs
bisectByParity = \case
  (x : xs) -> let (odds, evens) = bisectByParity xs in (x : evens, odds)
  [] -> ([], [])
trace1 v = trace (show v) v
infixl 0 |>
v |> f = f v

main = input >>= print . solve
