-- Brendan's Minesweeper Additions

module BMinesweeper where

import JMinesweeper
import System.IO
import System.Random

-- Board Generation --
-- Makes a board of size n x n, with all cells Blank and Covered
makeBoard :: Int -> Board
makeBoard n = combineRows n n

--makes n x n list of Rows ([Cell])
combineRows :: Int -> Int -> [[Cell]]
combineRows n 1 = [makeRow n 0 (n-1)]
combineRows n cnt = (makeRow n 0 (n-cnt)) : (combineRows n (cnt - 1))

-- makes the row y with locations at x, each Cell is Cell Blank Covered (y,x)
makeRow :: Int -> Int -> [Cell]
makeRow 0 x y = []
makeRow n x y = (Cell Blank Covered (y,x)) : (makeRow (n-1) (x+1) y)

testBoard = makeBoard 5

-- Bomb Generation --
-- Place bombs on the board randomly
-- TODO: should take the list of bombs in randLoc and look for those locations, and if it finds the location
    -- it should change that cell to be a bomb
-- placeBombs :: Board -> Board
--PUT FN here

-- gets a list of n location for bombs to be replaced (allows duplicates, that should be fixed)
randLoc :: Int -> Int -> [Location]
randLoc size n =
    do
        xg <- newStdGen
        yg <- newStdGen
        return (zip (take n (randomRs (0, size-1) yg)) (take n (randomRs (0, size-1) xg)))

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
