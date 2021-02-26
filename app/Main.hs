module Main where

import MData
import MGeneration
import Interaction
import MDisplay
import System.IO

{- How main will work
- we make the game (do all the initilizaion, with difficulty, board, etc.
- get the clicks and loop until game ends (lose or win)
-}

main :: IO ()
main = do
  putStrLn "Welcome to Minesweeper!\nPlease enter a difficulty (easy, medium, hard): "
  xg <- newStdGen --to pass to other functions and make game without making it IO?
  yg <- newStdGen -- same ^
  game <- createGameDiff --maybe edit for above reasons ^
  --display board
  putStrLn "Your board is ready to play!"
  newBoard <- getClick -- don't have access to all the right things, and should make a game?
  -- keep getting clicks?

-- creates a game with a difficulty provided as input
createGameDif = do
  difficulty <- getLine
  -- makes the game or resets main to get a propper difficulty
  if difficulty == "easy"
  then return makeGame Easy
  else if difficulty == "medium"
  then return makeGame medium
  else if difficulty == "hard"
  then return makeGame hard
  else
    putStrLn "Please enter a valid difficulty, one of easy, medium, or hard: "
    createGameDiff

-- gets a click from the user, verifies it is valid and remakes the board with the change
-- TODO: give this the game and add validations on the inputs
getClick game = do
  putStrLn "to click or (un)flag a location enter the coordinates, first enter an x: "
  x <- getLine
  if --check if x is in bound and convert to int?
  then getClick game
  else 
    putStrLn "now enter a y: "
    y <- getLine
    if --check if y is in bound and convert to int?
    then getClick game
    else 
      putStrLn "now enter a move (c or f): "
      move <- getLine
      if (move == "c")
      then return click game (x,y) --need to convert to digits
      else if (move == "f")
      then return flag game (x,y) --need to convert to digits
      else getClick game
  
--play function takes a game and will check the win (and lose) conditon on every call
-- if the game has been won or lost, return
play (Gamestate size bombs board winstate) = do
  if (winstate == Win)
  then putStrLn "You Win"
  else if (winstate == Loss)
  then putStrLn "You Lose"
  else
    --play the game
    --get an input from the player (getClick?) updatedGame <- getClick game
    --recursively call play with that new game play newgame

