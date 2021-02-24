{--
Basic data structures and functions for a game of minesweeper
--}
module MData where



data Game = Gamestate Size Bombs Board
-- Size is the x and y dimensions of the board
type Size = (Int, Int)
-- Bombs is the number of remaining bombs
type Bombs = Int
-- Board is a list of Rows
type Board = [Row]

-- Row is a list of Cells
type Row = [Cell]

-- each Cell is a cell with the contents of the cell
data Cell = CellC Content State Location-- add a location?, new type of either just Int or (Int,Int)
              deriving (Show, Eq)
-- Content is what a cell contains
data Content = Bomb |
               Int |
               Uninitialized
               deriving (Show,Eq)
-- if a cell is a clue, it says the number of bombs next to it.
type Clue = Int

data State = Covered |
             Flagged |
             Uncovered
             deriving (Show,Eq)

-- Location identifies a particular Cell on the board
type Location = (Int, Int)

getSize :: Board -> Size
getSize b = (length (b !! 0), length b)
