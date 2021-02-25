-- Brendan's Minesweeper Additions

module MGeneration where

import MData
import System.IO
import System.Random

-- Board Generation --
-- Makes a board of size x by y, with all cells Blank and Covered
makeBoard :: Size -> Board
makeBoard (width, height) = combineRows (width, height)
  where
      --makes cnt lists of Rows ([Cell]) of length n
      combineRows :: Size -> Board
      combineRows (n, 1)   = [makeRow n (0, 0)] -- TODO: should this be the base case, or _ 0 = []?
      combineRows (n, cnt) = (makeRow n (0, (n-cnt))) : (combineRows (n, (cnt - 1)))

-- makes the row y with locations at x, each Cell is Cell Blank Covered (x,y)
-- Int is a counter, (x, y) is location of next cell to make
makeRow :: Int -> Location -> Row
makeRow 0 (x, y) = []
makeRow n (x, y) = (CellC Blank Covered (x,y)) : (makeRow (n-1) ((x+1), y))

testBoard = makeBoard (5, 3)

{--
Place bombs on the board randomly
Takes Board,
      Number of bombs wanted,
      List of random locations,
      List of used
Returns a Board
To initialize, call with an empty list and the desired number of bombs
--}
placeBombs :: Board -> Int -> [Location] -> [Location] -> Board
placeBombs b 0 _ _ = b
placeBombs b n [] used = placeBombs b n (randLoc (getSize b) n) used
placeBombs b n (l:ls) used
  | l `elem` used = placeBombs b n ls used
  | otherwise = placeBombs (setContent b l Bomb) (n-1) ls (l:used)

-- gets a list of n location for bombs to be replaced (allows duplicates)
randLoc :: Size -> Int -> [Location]
randLoc (xsize, ysize) n =
    do
        xg <- newStdGen
        yg <- newStdGen
        return (zip
                  (take n (randomRs (0, xsize-1) xg))
                  (take n (randomRs (0, ysize-1) yg)))

-- takes a board without clues and returns a board with clues
placeClues :: Board -> Board
placeClues b = map (map calcClues) b
  where
    calcClues :: Cell -> Cell
    calcClues (CellC Blank s l) = (CellC (sumBombs (findNeighbors l (getSize b)))
                                         s l)
    calcClues cell = cell
    sumBombs :: [Location] -> Content
    sumBombs ls = Clue (foldl (\acc c -> if  c == Bomb then acc+1 else acc) 0
                              (map (getContent b) ls))
