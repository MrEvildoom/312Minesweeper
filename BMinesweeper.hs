-- Brendan's Minesweeper Additions

module BMinesweeper where

import JMinesweeper
import System.IO
import System.Random

{-- each Cell is a cell with the contents of the cell
data Cell = Cell Content State Location
              deriving (Show, Eq)
-}
-- Board Generation --
-- Makes a board of size n x n, with all cells Blank and Covered
-- makeBoard :: Int -> Board
makeBoard n = combineRows n n

--makes n x n list of Rows ([Cell])
combineRows n 1 = [makeRow n]
combineRows n cnt = (makeRow n) : (combineRows n (cnt - 1))

--makes a row ([Cell]) with all cells Blank and Covered
makeRow 0 = []
makeRow n = (Cell Blank Covered) : makeRow (n-1)

testBoard = makeBoard 5

-- Bomb Generation --
-- Place bombs on the board randomly
-- TODO: should take the list of bombs in randLoc and look for those locations, and if it finds the location
    -- it should change that cell to be a bomb
-- placeBombs :: Board -> Board
--PUT FN here

-- gets a list of n location for bombs to be replaced (allows duplicates, that should be fixed)
-- randLoc :: Int -> Int -> [Location]
randLoc size n =
    do
        xg <- newStdGen
        yg <- newStdGen    
        return (zip (take n (randomRs (0, size) xg)) (take n (randomRs (0, size) yg)))

{-
OLD IMPLEMENTATION
-- gets a list of n random locations for bombs to go // causing issues as No instance for (Show (IO (Integer, Integer))
getBombLoc size 1 = [randLoc size]
getBombLoc size n = (randLoc size) : (getBombLoc size (n-1)) 

-- makes a random location given the size of the board
-- randLoc :: Int -> Int -> Location
randLoc size = 
    do
        x <- randomRIO (0,size) 
        y <- randomRIO (0,size)
        return (x,y) 
-}


