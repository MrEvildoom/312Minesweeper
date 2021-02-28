module Interaction where

import MData
import System.IO
import Data.List

-- Pressing a cell
-- Is it revealed?/is it a flag?
-- Is it a bomb?
-- What is revealed
-- Change state of all to be revealed
-- Check for win condition

-- TODO: fix click to properly reveal 

clickGame:: Game -> Location -> Game
clickGame (Gamestate size bombs board winState) loc =
  checkCondition (Gamestate size bombs (click board loc) winState)
  where checkCondition g = checkLossCondition $ checkWinCondition g

click:: Board -> Location -> Board
click board loc = 
  if (shouldReveal board loc)
  then revealSpread board [loc] []
  else map (clickRow loc) board
    where clickRow ::Location -> Row -> Row
          clickRow loc row = map (\ (CellC cc cs cl) ->
                                  if cl == loc
                                  then clickCell (CellC cc cs cl)
                                  else (CellC cc cs cl))
                                row

--returns true if cell should reveal beightbors (Clue 0 and not already revealed)
shouldReveal:: Board -> Location -> Bool
shouldReveal b loc = 
  ((getContent b loc) == Clue 0) && ((getState b loc) == Covered)

--clicks a safe spot on the board for the user as a hint.
assistClick :: Game -> Game
assistClick (Gamestate size bombs board winState) = 
    clickGame (Gamestate size bombs board winState) (getSafeLocation (concat board))

getSafeLocation :: [Cell] -> Location
getSafeLocation [] = (-1,0) -- this should never happen
getSafeLocation ((CellC cc cs cl):t) 
     | (cc /= Bomb && cs == Covered) = cl
     | otherwise = getSafeLocation(t)

--flags a bomb for the user as a hint.
assistFlag :: Game -> Game
assistFlag (Gamestate size bombs board winState) = 
	flagGame(Gamestate size bombs board winState) (getBombLocation (concat board))

getBombLocation :: [Cell] -> Location
getBombLocation [] = (-1,0) -- this should never happen
getBombLocation ((CellC cc cs cl):t) 
     | (cc == Bomb && cs /= Flagged) = cl
     | otherwise = getBombLocation(t)

clickCell :: Cell -> Cell
clickCell (CellC cc cs cl) =
  if cs == Uncovered
  then (CellC cc cs cl)
  -- return a message saying that this location is already revealed
  else if cs == Flagged
  then (CellC cc cs cl)
  -- return a message saying that this location has been flagged, must be unflagged to uncover.
  else if cc == Bomb
  then (CellC cc Uncovered cl) --TODO: should these be different?
  -- when we check loss condition, it should say lost now (Bomb is uncovered)
  else (CellC cc Uncovered cl)
    -- this needs to also somehow trigger revealSpread
    -- uncover this cell, if the board is a win, checking the board will cahnge state.

-- check win condition, if not met then reach just reveal the board spread.
-- the win condition: # of non-bomb cells revealed + # of remaining uncovered bomb tiles = total tiles on the board.
-- if all bombs are flagged, a win is achieved!
checkWinCondition :: Game -> Game
checkWinCondition (Gamestate size bombs board winState) =
  if (any unrevNonbomb (concat board) && not (checkAllBombsFlagged (concat board)))
  then (Gamestate size bombs board winState)
  else (Gamestate size bombs board Win)
  where
    unrevNonbomb :: Cell -> Bool
    unrevNonbomb (CellC (Clue _) Covered _) = True
    unrevNonbomb _ = False
  {--
  if ((countBombsFn (l:ls)) + (countRevealedCells (l:ls))) == (length l) * (length ls)
  then (Gamestate size bombs (l:ls) Win)
  -- TODO: win condition, terminate the function
  else (Gamestate size bombs (l:ls) winState)
  --}

checkAllBombsFlagged :: [Cell] -> Bool
checkAllBombsFlagged b = 
	foldr (&&) True (map (\ (CellC cc cs cl) ->
                                cc == Bomb && cs == Flagged) 
								(filter (\ (CellC cc cs cl) ->
                                         cc == Bomb) b))

-- if there are any uncovered bombs, the game is lost
checkLossCondition :: Game -> Game
checkLossCondition (Gamestate size bombs board winState) =
  if (any revBomb (concat board))
  then (Gamestate size bombs board Loss)
  else (Gamestate size bombs board winState)
  where
    revBomb :: Cell -> Bool
    revBomb (CellC Bomb Uncovered _) = True
    revBomb _ = False


-- should count all the bombs on the board
countBombsFn :: Board -> Location -> Int
countBombsFn board loc = sum [1 | row <- board,
                                  cell <- row,
                                  (getContent board loc) == Bomb]

-- should count all the currently revealed cells on the board
countRevealedCells :: Board -> Location -> Int
countRevealedCells board loc = sum [1 | row <- board,
                                        cell <- row,
                                        (getState board loc) == Uncovered]

-- FLAGGING A CELL --
--flags the locaiton and updates the game
--TODO: make flagging a cell decrement the number of bombs and unflagging a cell increment
flagGame:: Game -> Location -> Game
flagGame (Gamestate size bombs board winstate) loc =
  checkWinCondition(Gamestate size bombs (flag board loc) winstate)

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