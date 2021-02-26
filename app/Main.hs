module Main where

import MData
import MGeneration
import Interaction
import MDisplay
import System.IO

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


getClick = do
  putStrLn "to click or (un)flag a location enter the coordinates, first enter an x: "
  x <- getLine
  if --check if x is in bound
  then getClick
  else 
    putStrLn "now enter a y: "
    y <- getLine
    if --check if y is in bound
    then getClick
    else 
      putStrLn "now enter a move (c or f): "
      move <- getLine
      if (move == "c")
      then return click board (x,y) --need to convert to digits and get board?
      else if (move == "f")
      then return flag board (x,y) --need to convert to digits and get board?
      else getClick
  