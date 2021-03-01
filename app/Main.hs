module Main where

import MData
import MGeneration
import Interaction
import MDisplay
import System.IO
import System.Random
import Data.List
import Data.Maybe (fromJust)
import Data.Char

{- How main will work
- we make the game (do all the initilizaion, with difficulty, board, etc.
- get the clicks and loop until game ends (lose or win)
-}

main :: IO ()
main = do
  putStrLn "Welcome to Minesweeper!\nPlease enter a difficulty (very easy, easy, medium, hard): "
  xg <- newStdGen
  yg <- newStdGen
  game <- createGameDiff (xg, yg)
  --display game
  putStrLn "Your board is ready to play!"
  putStrLn "to quit (and lose), enter \"quit\"\n"
  endedGame <- play game
  putStrLn "Thanks for playing!"
  putStrLn "do you want to play again? y or yes for yes, anything else for no"
  again <- getLine
  let lagain = map toLower again
  if lagain == "y" || lagain == "yes"
  then main
  else putStrLn "Please come again soon! \nLeaving game..."

-- creates a game with a difficulty provided as input
createGameDiff gens = do
  difficulty <- getLine
  let ldifficulty = map toLower difficulty
  -- makes the game or resets main to get a propper difficulty
  if ldifficulty == "very easy"
  then return (makeGame VeryEasy gens)
  else if ldifficulty == "easy"
  then return (makeGame Easy gens)
  else if ldifficulty == "medium"
  then return (makeGame Medium gens)
  else if ldifficulty == "hard"
  then return (makeGame Hard gens)
  else do
    putStrLn "Please enter a valid difficulty, one of easy, medium, or hard: "
    createGameDiff gens

--play function takes a game and will check the win (and lose) conditon on every call
-- if the game has been won or lost, return
play (Gamestate size bombs board winstate) = do
  display (Gamestate size bombs board winstate)
  if (winstate == Win)
  then do
    putStrLn "You Win"
    return (Gamestate size bombs board winstate)
  else do
    -- Loss
    if winstate == Loss
    then do
        putStrLn "You Lose"
        return (Gamestate size bombs board winstate)
    -- Input moves
    else do
        putStrLn "\nWhat move would you like to make flag (\"f\"), click (\"c\"), or receive a hint (\"h\")?"
        move <- getLine
        let lmove = map toLower move
        -- quitting
        if lmove == "quit"
        then do
            putStrLn "quitting game..."
            return (Gamestate size bombs board Loss)
        else do
            -- flag
            if lmove == "f"
            then do
                updatedGame <- doFlag (Gamestate size bombs board winstate)
                play updatedGame
            -- click
            else if lmove == "c"
            then do
                updatedGame <- doClick (Gamestate size bombs board winstate)
                play updatedGame
            -- hint
            else if lmove == "h"
            then do
                updatedGame <- doHint (Gamestate size bombs board winstate)
                play updatedGame
            else do
                putStrLn "Please enter a valid move: \"f\", \"c\", or \"quit\""
                play (Gamestate size bombs board winstate)

-- performs the assistant action
doHint game = do
  putStrLn "\nWould you like to have a bomb flagged for you (\"f\"),or have a safe spot clicked for you (\"c\")? , or \"no\" to leave assistant"
  hint <- getLine
  let lhint = map toLower hint
  -- flag hint
  if lhint == "f"
  then do
    updatedGame <- flagBombLocation game
    return updatedGame
  -- click hint
  else if lhint == "c"
  then do
    updatedGame <- assistClickLocation game
    return updatedGame
  else if lhint == "no"
  then do return game
  else do
    putStrLn "Please enter a valid hint request, a bomb flagged or a safe spot clicked: \"f\", \"c\", or \"no\" to leave assistant"
    doHint game

--flow of asking for a hint
{-askForHint (Gamestate size bombs board winstate) = do
  putStrLn "\nWould you like to have a bomb flagged for you (\"f\"),or have a safe spot clicked for you (\"c\")?"
  hint <- getLine
  if (hint == "f")
  then do
    updatedGame <- flagBombLocation (Gamestate size bombs board winstate)
    play updatedGame
  else if (hint == "c")
  then do
  updatedGame <- assistClickLocation (Gamestate size bombs board winstate)
  play updatedGame
  else do
    putStrLn "Please request a valid hint, \"f\" to flag a bomb, \"c\" to click a safe spot, or \"quit\""
  askForHint (Gamestate size bombs board winstate)-}

--flagBombLocation will flag a bomb on the board for the user (currently flags only one bomb)
flagBombLocation (Gamestate size bombs board winstate) = do
  putStrLn "\nFlagged a bomb for you!!"
  return (assistFlag (Gamestate size bombs board winstate))

--assistClickLocation will click a safe place on the board for the user
assistClickLocation (Gamestate size bombs board winstate) = do
  putStrLn "\nClicked a safe spot for you!!"
  return (assistClick (Gamestate size bombs board winstate))

--doFlag will perform a flagging action on the location provided
doFlag (Gamestate size bombs board winstate) = do
  putStrLn "\nTo flag, we need a location."
  loc <- getLoc (Gamestate size bombs board winstate)
  flagMsg board loc
  return (flagGame (Gamestate size bombs board winstate) loc)

--doClick will perform the click action on the location provided
doClick (Gamestate size bombs board winstate) = do
  putStrLn "\nTo click we need a location."
  loc <- getLoc (Gamestate size bombs board winstate)
  clickMsg board loc
  return (clickGame (Gamestate size bombs board winstate) loc)

--getLoc helper for doFlag and doClick that gets a vlaid location from the user given a certain game
getLoc game = do
  putStrLn "Please enter your x-coordinate"
  x <- getLine
  let lx = map toLower x
  if validx lx game
  then do
    putStrLn "Please enter your y-coordinate"
    y <- getLine
    let ly = map toLower y
    if validy ly game
    then return (convert lx, convert ly)
    else getLoc game
  else getLoc game

--valid takes a coordinate and game and verifies if it is valid
validx:: String -> Game -> Bool
--valid coord game = True
validx coord (Gamestate (xsz, ysz) bombs board winstate) =
  if (length coord == 1) && ((coord!!0) `elem` (take (xsz) ['a'..'z']))
  then True
  else False

validy:: String -> Game -> Bool
--valid coord game = True
validy coord (Gamestate (xsz, ysz) bombs board winstate) =
  if (length coord == 1) && ((coord!!0) `elem` (take (ysz) ['a'..'z']))
  then True
  else False

--convert takes a string letter and converts to the correct int coordination representation
convert:: String -> Int
convert coord = fromJust $ elemIndex (coord!!0) ['a'..'z']

--determines what message to send after a move (if flag, bomb, or already revealed)
clickMsg board loc = do
  if (getState board loc) == Uncovered
  then do revealMsg
  else do
    if (getState board loc) == Flagged
    then do clickFlagMsg
    else do
      if (getContent board loc) == Bomb
      then do bombMsg
      else do putStrLn "You found a valid space, good job!\n"


--prints a message saying location is flagged
clickFlagMsg = do
  putStrLn "This cell is flagged, please unflag before clicking\n"

--prints a message indicating a bomb was pressed
bombMsg = do
  putStrLn "You pressed a bomb..\n"

--prints a message stating that the locaiton is already revealed
revealMsg = do
  putStrLn "This cell is already revealed, try another one\n"

--prints a message saying if the cell was flagged, unflagged, or could not be flagged
flagMsg board loc = do
  if (getState board loc) == Uncovered
  then do revealMsg
  else do
    if (getState board loc) == Flagged
    then do unflagMsg
    else do flagingMsg


unflagMsg = do
  putStrLn "You unflagged the cell at the specified location\n"

flagingMsg = do
  putStrLn "You flagged the cell at the specified location\n"
