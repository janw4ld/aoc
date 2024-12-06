{- HLINT ignore "Use map once" -}
module Main (main) where

import Data.Function
import Data.List (isInfixOf, (!?))
import Data.Maybe

-- input = readFile "./test-input"

input = readFile "./input"
solve = part2 . prepInput

part1 grid =
  sum
    [ scanForXMAS grid x y
    | (y, line) <- zip [0 ..] grid
    , (x, char) <- zip [0 ..] line
    , char == anchor
    ]
 where
  anchor = 'X'

part2 grid =
  length . filter id $
    [ isCrossMAS grid x y
    | (y, line) <- zip [0 ..] grid
    , (x, char) <- zip [0 ..] line
    , char == anchor
    ]
 where
  anchor = 'A'

prepInput = lines

isCrossMAS grid x y =
  ("MMSS" `isInfixOf`)
    . ((<>) <*> id)
    . catMaybes
    $ [ grid !? y' >>= (!? x')
      | (dx, dy) <- deltas
      , let
          x' = x + dx
          y' = y + dy
      ]
 where
  -- order matters here, has to be clockwise
  deltas = [(1, 1), (1, -1), (-1, -1), (-1, 1)]

scanForXMAS grid x y =
  length
    [ ()
    | (dx, dy) <- deltas
    , and $
        [ grid !? y' >>= (!? x') & (== Just c)
        | (c, s) <- steps
        , let
            x' = x + dx * s
            y' = y + dy * s
        ]
    ]
 where
  steps = [('M', 1), ('A', 2), ('S', 3)]
  -- deltas = [(0, 1), (0, -1), (1, 0), (-1, 0), (1, 1), (-1, -1), (1, -1), (-1, 1)]
  deltas = [(dx, dy) | dx <- [-1 .. 1], dy <- [-1 .. 1], (dx, dy) /= (0, 0)]

main = input >>= print . solve
