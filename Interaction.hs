module Interaction where

import MData
import System.IO

-- Pressing a cell
-- Is it revealed?/is it a flag?
-- Is it a bomb?
-- What is revealed
-- Change state of all to be revealed
-- Check for win condition
clickCell :: Board -> Location -> Board
  

-- FLAGGING A CELL --
-- given a board and location, flag the location, or unflag if it is already flagged
-- if the cell at loc is revealed then do nothing
flag :: Board -> Location -> Board
flag b loc = map (flagRow loc)
  where flagRow :: Location -> Row -> Row
        flagRow = map (\ (CellC cc cs cl) -> 
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

revealSpread :: Board -> [Location] -> [Location] -> Board
revealSpread b [] _ = b
revealSpread b (l:ls) oldls 
  | l `elem` oldls = revealSpread b ls oldls -- discard if revealed
  | otherwise = revealSpread (setState b l Uncovered)
                             getRevNeighbors ++ ls  
                             l:oldls
  where
    getRevNeighbors = filter revealable ((findNeighbors l (getsize b)))
    -- cells are revealable if they are covered, not flagged, and a clue or blank
    revealable :: Location -> Bool 

-- Helper Functions

getContent :: Board -> Location -> Content
getContent b (x,y) = (\(CellC c _ _) -> )b !! y

-- Given a board, a location and Content, update the cell content
setState :: Board -> Location -> State -> Board
setState b loc state = map (setStateRow loc state) b
  where setStateRow :: Location -> State -> Row -> Row
        setStateRow loc state row =
          map (\(CellC cc cs cl) -> if cl == loc
            then (CellC cc state cl)
            else (CellC cc cs cl)) row