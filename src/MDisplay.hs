-- functions for displaying Minesweeper in a text interface

module MDisplay where

import MData
import MInteraction
import Data.List

{--
Displays, when printed, look like this

  00 Bombs   -- infoLine
  a b c d e  -- topline
  ---------
a|X F 1 . .
b|X X 2 . .
c|X F 1 1 1
d|X X X X X
e|X X X X X

input "(x, y, c)"
   or "(x, y, r)"

X is a covered square
numbers are clues
F is a flag
--}

display :: Game -> IO ()
display g = putStrLn ((makeInfoLine     g) ++
                      (makeTopLine      g) ++
                      (makeDisplayBoard g))

makeInfoLine :: Game -> [Char]
makeInfoLine (Gamestate _ bombs board _) =
    "Unrevealed Cells: " ++ (show (countUnrevealedCells board)) ++
    " | Bombs: " ++ (show bombs) ++ 
    " | Flagged Cells: " ++ (show (countFlaggedCells board)) ++
    "\n"


-- something like length filter isflag? concat board
makeTopLine :: Game -> [Char]
makeTopLine (Gamestate (x, _) _ _ _) =
    ' ':' ':
    (intersperse ' ' (take x ['a'..])) ++
     "\n"

makeDisplayBoard :: Game -> [Char]
makeDisplayBoard (Gamestate (x, y) bombs board _) = flatBoard
  where
    charBoard    :: [[Char]]
    zippedBoard  :: [(Char, [Char])]
    splicedBoard :: [[Char]]
    spreadBoard  :: [[Char]]
    linedBoard   :: [[Char]]
    flatBoard    :: [Char]
    charBoard    = map (map cellToChar) board      -- convert to chars
    spreadBoard  = map (intersperse ' ') charBoard -- add spaces
    zippedBoard  = zip ['a'..] spreadBoard
    splicedBoard = foldr (\pair acc -> (compressPair pair):acc) [] zippedBoard

    linedBoard   = intersperse "\n" splicedBoard
    flatBoard    = concat linedBoard

cellToChar :: Cell -> Char
cellToChar (CellC _ Covered _)          = 'X'
cellToChar (CellC _ Flagged _)          = 'F'
cellToChar (CellC Bomb Uncovered _)     = '!'
cellToChar (CellC (Clue 0) Uncovered _) = '.'
cellToChar (CellC (Clue c) Uncovered _) = (show c) !! 0

compressPair :: (Char, [Char]) -> [Char]
compressPair (char, str) = char:'|':str
