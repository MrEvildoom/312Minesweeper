-- functions for displaying Minesweeper in a text interface

module MDisplay where

import MData
import Data.List

{--
Displays, when printed, look like this

  00 Bombs   -- infoLine
  a b c d e  -- topline
a X X F 1 0
b X X 2 0 0
c X F 1 1 1
d X X X X X
e X X X X X

input "(x, y, c)"
   or "(x, y, r)"

X is a covered square
numbers are clues
F is a flag
--}

display :: Game -> IO ()
display g = putStrLn ((makeInfoLine     g) ++
                      (makeTopLine      g) ++ ['\n']
                      (makeDisplayBoard g))

makeInfoLine :: Game -> [Char]
makeInfoLine (Gamestate _ bombs _) = (show bombs) ++ " Bombs \n"

makeTopLine :: Game -> [Char]
makeTopLine (Gamestate (x, _) _ _) = ' ':' ':(intersperse ' ' (take x ['a'..]))

makeDisplayBoard :: Game -> [Char]
makeBoard (Gamestate (x, y) bombs board) = flatBoard
  where
    charBoard    = map $ map cellToChar board
    zippedBoard  = zip ['a'..] charBoard
    splicedBoard = foldr (\(label, row) acc -> (label:row):acc) zippedBoard []
    spreadBoard  = map (intersperse ' ') splicedBoard
    linedBoard   = intersperse ['/n'] splicedBoard
    flatBoard    = concat linedBoard


cellToChar :: Cell -> Char
cellToChar (Cell _ Covered _)          = 'X'
cellToChar (Cell _ Flagged _)          = 'F'
cellToChar (Cell (Clue 0) Uncovered _) = '0'
cellToChar (Cell (Clue c) Uncovered _) = (show c) !! 0
