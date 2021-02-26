module Interaction where

import MData
import System.IO

-- Pressing a cell
-- Is it revealed?/is it a flag?
-- Is it a bomb?
-- What is revealed
-- Change state of all to be revealed
-- Check for win condition
--clickCell :: Board -> Location -> Board

--TODO chance clicking a cell to work with a game and not board

-- Pressing a cell --
-- given a board and location, find the cell on the board to operate on.
-- if the cell at loc is revealed then do nothing
click :: Board -> Location -> Board
click b loc = map (clickRow loc) b
  where clickRow ::Location -> Row -> Row
        clickRow loc row = map (\ (CellC cc cs cl) ->
            if cl == loc
            then clickCell (CellC cc cs cl) b loc
            else (CellC cc cs cl)) row

-- given a Cell,
-- if it is already revealed, do nothing
-- What to do if it is flagged? assume that it must be unflagged to uncover the cell.
-- I assume when we discover a bomb, we want to reset the state of the board after outputting a game over message.
clickCell :: Cell -> Board -> Location -> Board
clickCell (CellC cc cs cl) b loc =
  if cs == Uncovered
  then b 	-- return a message saying that this location is already revealed
  else if cs == Flagged
  then b 	-- return a message saying that this location has been flagged, must be unflagged to uncover.
  else if cc == Bomb
  then b 	-- TODO: game over. Return to original I/O console message. 
  else if cc != Bomb       
  then checkWinCondition (Cell C cc cs cl) (revealSpread	b (loc:[]) []) loc)	-- uncover this cell and change the state of everything that should be revealed.
  else (CellC cc cs cl)
  
-- check win condition, if not met then reach just reveal the board spread.
-- the win condition: # of non-bomb cells revealed + # of remaining uncovered bomb tiles = total tiles on the board.
checkWinCondition :: Cell -> Board -> Board
checkWinCondition (CellC cc cs cl) (l:ls) =
  if ((countBombsFn (l:ls)) + (countRevealedCells (l:ls))) == (length l) * (length ls)
  then b -- TODO: win condition, terminate the function
  else b

-- should count all the bombs on the board
countBombsFn :: Board -> Location -> Num
countBombs xss = sum [1 | xs <- xss, x <- xs, (getContent xss (x location)) == Bomb] 
-- should count all the currently revealed cells on the board
countRevealedCells :: Board -> Location -> Num
countRevealedCells xss = sum [1 | xs <- xss, x <- xs, (getState xss (x location)) == Uncovered] 




-- FLAGGING A CELL --
--TODO make work with game not board

flagGame 

-- given a board and location, flag the location, or unflag if it is already flagged
-- if the cell at loc is revealed then do nothing
flag :: Board -> Location -> Board
flag b loc = map (flagRow loc) b
  where flagRow ::Location -> Row -> Row
        flagRow loc row = map (\ (CellC cc cs cl) ->
            if cl == loc
            then flagCell (CellC cc cs cl)
            else (CellC cc cs cl)) row

-- given a Cell, set its state to flagged (or unflag if flagged) if it is not revealed
flagCell :: Cell -> Cell
flagCell (CellC cc cs cl) =
  if cs == Uncovered
  then (CellC cc cs cl) --in game could output message "already revealed"
  else if cs == Flagged
  then (CellC cc Covered cl)
  else (CellC cc Flagged cl)

-- takes a board and a location of a blank cell, returns a board with cells around it revealed
revealSpread :: Board -> [Location] -> [Location] -> Board
revealSpread b [] _ = b
revealSpread b (l:ls) oldls
  | l `elem` oldls = revealSpread b ls oldls -- discard if revealed
  | (getContent b l) == Clue 0 = revealSpread (setState b l Uncovered)
                            -- uncover cell
                            (getRevNeighbors ++ ls)
                             -- add revealable neighbors to ls
                            (l:oldls) -- mark l as revealed
  | otherwise = revealSpread (setState b l Uncovered) ls (l:oldls)
  where
    getRevNeighbors :: [Location]
    getRevNeighbors = filter revealable ((findNeighbors l (getSize b)))
    -- cells are revealable if they are covered, not flagged, and a clue
    revealable :: Location -> Bool
    revealable loc = ((getContent b loc) /= Bomb) && ((getState b loc) == Covered)

-- Helper Functions
