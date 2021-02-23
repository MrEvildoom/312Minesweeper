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

{-- TODO
Place bombs on the board randomly
Takes a Board, nNumber of bombs wanted, List of random locations, list of used locations, and returns a Board
To initialize, call with an empty list and the desired number of bombs
--}
placeBombs :: Board -> Int -> [Location] -> [Location] -> Board
placeBombs b 0 _ _ = b
placeBombs b n [] used = placeBombs b n (randLoc (getSize b) n) used
placeBombs b n l:ls used =
  | l `elem` used = placeBombs b n ls used
  | otherwise = placeBombs (setContent b l Bomb) n-1 ls l:used

-- sets the cell in a board at location to content
-- TODO this does not work
setContent :: Board -> Location -> Content -> Board
-- setContent b (x, y) c = (\(Cell _ s l) -> Cell c s l) ((b !! y) !! x)
setContent b l c = b

-- gets a list of n location for bombs to be replaced (allows duplicates, that should be fixed)
randLoc :: Size -> Int -> [Location]
randLoc (xsize, ysize) n =
    do
        xg <- newStdGen
        yg <- newStdGen
        return (zip (take n (randomRs (0, xsize-1) xg)) (take n (randomRs (0, ysize-1) yg)))

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
-- Helper Functions

-- gets a board's size so we don't have to always give size as an argument
getSize :: Board -> Size
getsize b = (length (b !! 0), length b)
