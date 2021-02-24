-- Brendan's Minesweeper Additions

module MGeneration where

import MData
import System.IO
import System.Random

-- Board Generation --
-- Makes a board of size n x n, with all cells Blank and Covered
makeBoard :: Size -> Board
makeBoard (width, height) = combineRows width height

--makes n x n list of Rows ([Cell])
combineRows :: Int -> Int -> [[Cell]]
combineRows n 1 = [makeRow n 0 (n-1)]
combineRows n cnt = (makeRow n (0, (n-cnt))) : (combineRows n (cnt - 1))

-- makes the row y with locations at x, each Cell is Cell Blank Covered (x,y)
-- Int is a counter, size is location of next cell to make
makeRow :: Int -> Location -> [Cell]
makeRow 0 (x, y) = []
makeRow n (x, y) = (CellC Blank Covered (x,y)) : (makeRow (n-1) ((x+1), y))

testBoard = makeBoard 5 3

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
placeBombs b n (l:ls) used
  | l `elem` used = placeBombs b n ls used
  | otherwise = placeBombs (setContent b l Bomb) n-1 ls l:used



-- gets a list of n location for bombs to be replaced (allows duplicates, that should be fixed)
randLoc :: Size -> Int -> [Location]
randLoc (xsize, ysize) n =
    do
        xg <- newStdGen
        yg <- newStdGen
        return (zip (take n (randomRs (0, xsize-1) xg)) (take n (randomRs (0, ysize-1) yg)))

-- Helper Functions
-- Given a row and a x location and Content, update the cell
setContent :: Board -> Location -> Content -> Board
setContent b loc content = map (setContentRow loc content) b
  where setContentRow :: Location -> Content -> Row -> Row
        setContentRow loc content row =
          map (\(CellC cc cs cl) -> if cl == loc
            then (CellC content cs cl)
            else (CellC cc cs cl)) row

-- gets a board's size so we don't have to always give size as an argument
getSize :: Board -> Size
getsize b = (length (b !! 0), length b)
