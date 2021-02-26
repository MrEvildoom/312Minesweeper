-- Brendan's Minesweeper Additions

module MGeneration where

import MData
import System.IO
import System.Random
import Data.List

-- Takes a difficulty and makes a game of the corresponding difficulty
makeGame :: Difficulty -> Game
makeGame Easy   = placeClues.placeBombs (Gamestate (10, 8)  10 (makeBoard (10, 8)))
makeGame Medium = placeClues.placeBombs (Gamestate (18, 14) 40 (makeBoard (18, 14)))
makeGame Hard   = placeClues.placeBombs (Gamestate (24, 20) 99 (makeBoard (24, 20)))

-- Board Generation --
-- Makes a board of size x by y, with all cells Blank and Covered
makeBoard :: Size -> Board
makeBoard (width, height) = combineRows (width, height) height
  where
      --makes cnt lists of Rows ([Cell]) of length n
      combineRows :: Size -> Int -> Board
      combineRows (w, h) 1   = [makeRow w (0, (h-1))] -- TODO: should this be the base case, or _ 0 = []?
      combineRows (w, h) cnt = (makeRow w (0, (h-cnt))) : (combineRows (w, h) (cnt - 1))

-- makes the row y with locations at x, each Cell is Cell Blank Covered (x,y)
-- Int is a counter, (x, y) is location of next cell to make
makeRow :: Int -> Location -> Row
makeRow 0 (x, y) = []
makeRow n (x, y) = (CellC Blank Covered (x,y)) : (makeRow (n-1) ((x+1), y))

testBoard = makeBoard (5, 4)


placeBombs :: Game -> Game
placeBombs (Gamestate size bombs board)
             = (Gamestate size bombs (boardPlaceBombs board size bombs [] []))
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
boardPlaceBombs :: Board -> Size -> Int -> [Location] -> [Location] -> Board
boardPlaceBombs b _ 0 _ _ = b
boardPlaceBombs b s n [] used = boardPlaceBombs b s n (randLoc s n) used
boardPlaceBombs b s n (l:ls) used
  | l `elem` used = boardPlaceBombs b s n ls used
  | otherwise     = boardPlaceBombs (setContent b l Bomb) s (n-1) ls (l:used)

-- gets a list of n location for bombs to be replaced (allows duplicates)
randLoc :: Size -> Int -> [Location]
randLoc (xsize, ysize) n = (take n (zip [1..] [1..]))
     --TODO this is just so the program compiles. Fix this!

newRandLoc (xsize, ysize) n = 
  do
    xg <- newStdGen
    yg <- newStdGen
    return (listLoc xg yg (0, (xsize-1)) (0, (ysize-1)) n)


listLoc xg yg szx szy n = take n (nub (zip (randomRs szx xg) (randomRs szy yg)))

{--
    do
        xg <- newStdGen
        yg <- newStdGen
        return (zip
                  (take n (randomRs (0, xsize-1) xg))
                  (take n (randomRs (0, ysize-1) yg)))
--}

placeClues :: Game -> Game
placeclues (Gamestate size bombs board) = (Gamestate size bombs (boardPlaceClues size board))

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
