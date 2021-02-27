-- Brendan's Minesweeper Additions

module MGeneration where

import MData
import System.IO
import System.Random
import Data.List

-- Takes a difficulty and makes a game of the corresponding difficulty
makeGame :: Difficulty -> (StdGen, StdGen) -> Game
makeGame Easy   gens = placeClues.placeBombs (Gamestate (10, 8)  10 (makeBoard (10, 8)) Continue) gens
makeGame Medium gens = placeClues.placeBombs (Gamestate (18, 14) 40 (makeBoard (18, 14)) Continue) gens
makeGame Hard   gens = placeClues.placeBombs (Gamestate (24, 20) 99 (makeBoard (24, 20)) Continue) gens

-- Board Generation --
-- Makes a board of size x by y, with all cells Blank and Covered
makeBoard :: Size -> Board
makeBoard (width, height) = combineRows (width, height) height
  where
      --makes cnt lists of Rows ([Cell]) of length n
      combineRows :: Size -> Int -> Board
      combineRows (w, h) 1   = [makeRow w (0, (h-1))] 
      combineRows (w, h) cnt = (makeRow w (0, (h-cnt))) : (combineRows (w, h) (cnt - 1))

-- makes the row y with locations at x, each Cell is Cell Blank Covered (x,y)
-- Int is a counter, (x, y) is location of next cell to make
makeRow :: Int -> Location -> Row
makeRow 0 (x, y) = []
makeRow n (x, y) = (CellC Blank Covered (x,y)) : (makeRow (n-1) ((x+1), y))

testBoard = makeBoard (5, 4)


placeBombs :: Game -> (StdGen, StdGen) -> Game
placeBombs (Gamestate size bombs board winstate) gens
             = (Gamestate size bombs (boardPlaceBombs board size bombs [] [] gens) winstate)
{--
Place bombs on the board randomly
Takes Board,
      Board Size,
      Number of bombs wanted,
      List of random locations,
      List of used
Returns a Board
To initialize, call with an empty list and the desired number of bombs
--}
boardPlaceBombs :: Board -> Size -> Int -> [Location] -> [Location] -> (StdGen, StdGen) -> Board
boardPlaceBombs b _ 0 _ _ _ = b
boardPlaceBombs b s n [] used gens = boardPlaceBombs b s n (listLoc gens s n) used gens
boardPlaceBombs b s n (l:ls) used gens
  | l `elem` used = boardPlaceBombs b s n ls used gens
  | otherwise     = boardPlaceBombs (setContent b l Bomb) s (n-1) ls (l:used) gens

-- gets a list of n location for bombs to be replaced (allows duplicates)
randLoc :: Size -> Int -> [Location]
randLoc (xsize, ysize) n = (take n (zip [1..] [1..]))
     --TODO this is just so the program compiles. Fix this!

{-newRandLoc (xsize, ysize) n = 
  do
    xg <- newStdGen
    yg <- newStdGen
    return (listLoc xg yg (0, (xsize-1)) (0, (ysize-1)) n)
-}
--the new randloc that takes generators as an input and outputs n unique random locations
listLoc (xg, yg) (szx, szy) n = take n (nub (zip (randomRs (0, szx-1) xg) (randomRs (0, szy-1) yg)))

{--
    do
        xg <- newStdGen
        yg <- newStdGen
        return (zip
                  (take n (randomRs (0, xsize-1) xg))
                  (take n (randomRs (0, ysize-1) yg)))
--}

placeClues :: Game -> Game
placeclues (Gamestate size bombs board winstate) = (Gamestate size bombs (boardPlaceClues size board) winstate)

-- takes a board without clues and returns a board with clues
boardPlaceClues :: Size -> Board -> Board
boardPlaceClues size b = map (map calcClues) b
  where
    calcClues :: Cell -> Cell
    calcClues (CellC Blank state loc) = (CellC (sumBombs (findNeighbors loc size))
                                         state loc)
    calcClues cell = cell
    sumBombs :: [Location] -> Content
    sumBombs ls = Clue (foldl (\acc c -> if  c == Bomb then acc+1 else acc) 0
                              (map (getContent b) ls))
