module Main where

import MData
import MGeneration
import Interaction
--import MDisplay
import System.IO
import System.Random
import Data.List

{- How main will work
- we make the game (do all the initilizaion, with difficulty, board, etc.
- get the clicks and loop until game ends (lose or win)
-}

main :: IO ()
main = do
  putStrLn "Welcome to Minesweeper!\nPlease enter a difficulty (easy, medium, hard): "
  xg <- newStdGen
  yg <- newStdGen
  game <- createGameDiff (xg, yg) 
  --display board
  putStrLn "Your board is ready to play!"
  putStrLn "to quit (and lose), enter \"quit\""
  endedGame <- play game
  putStrLn "Thanks for playing!"
  putStrLn "do you want to play again? y or yes for yes, anything else for no"
  again <-getLine
  if again == "y" || again == "yes"
  then main
  else putStrLn "Please come again soon! \n Leaving game..."

-- creates a game with a difficulty provided as input
createGameDiff gens = do
  difficulty <- getLine
  -- makes the game or resets main to get a propper difficulty
  if difficulty == "easy"
  then return (makeGame Easy gens)
  else if difficulty == "medium"
  then return (makeGame Medium gens)
  else if difficulty == "hard"
  then return (makeGame Hard gens)
  else do
    putStrLn "Please enter a valid difficulty, one of easy, medium, or hard: "
    createGameDiff gens
  
--play function takes a game and will check the win (and lose) conditon on every call
-- if the game has been won or lost, return
play (Gamestate size bombs board winstate) = do
  if (winstate == Win)
  then do
    putStrLn "You Win"
    return (Gamestate size bombs board winstate)
  else do
    if winstate == Loss
    then do
        putStrLn "You Lose"
        return (Gamestate size bombs board winstate)
    else do
    --Show board here?
        putStrLn (show board)
        putStrLn "What move would you like to make flag (\"f\") or click (\"c\")?"
        move <- getLine
        if move == "quit"
        then do
            putStrLn "quitting game..." 
            return (Gamestate size bombs board Loss)
        else do 
            if move == "f"
            then do 
                updatedGame <- doFlag (Gamestate size bombs board winstate)
                play updatedGame
            else if move == "c"
            then do 
                updatedGame <- doClick (Gamestate size bombs board winstate)
                play updatedGame
            else do
                putStrLn "Please enter a valid move: \"f\", \"c\", or \"quit\""
                play (Gamestate size bombs board winstate)
    -- recursively call play to get next move with the updated game
        --play updatedGame

--doFlag will perform a flagging action on the location provided
doFlag game = do 
  putStrLn "To flag, we need a location."
  loc <- getLoc game
  return game --flagGame game loc

--doClick will perform the click action on the location provided
doClick game = do
  putStrLn "To click we need a location."
  loc <- getLoc game
  return game --clickGame game loc

--getLoc helper for doFlag and doClick that gets a vlaid location from the user given a certain game
getLoc game = do
  putStrLn "Please enter your x-coordinate"
  x <- getLine
  if valid x game
  then do
    putStrLn "Please enter your y-coordinate"
    y <- getLine
    if valid y game
    then return (convert x, convert y)
    else getLoc game
  else getLoc game

--valid takes a coordinate and game and verifies if it is valid
valid:: String -> Game -> Boolean
valid coord game = True

--convert takes a string letter and converts to the correct int coordination representation
convert:: String -> Int
convert coord = 1