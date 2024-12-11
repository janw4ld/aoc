module Main (main) where

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

part1 = length . filter isSafe . map diffEachTwo

prepInput = map (map (read @Int) . words) . lines

isSafe l = all not (markSignChanges l) && all inSafeRange l
inSafeRange x = abs x > 0 && abs x <= 3

markSignChanges = zipEachTwoWith (\x y -> x * y < 0)
diffEachTwo = zipEachTwoWith (-)
zipEachTwoWith f xs = zipWith f xs (tail xs)
deleteAt xs n = take n xs <> drop (n + 1) xs

main = input >>= print . solve
