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
  putStrLn "Welcome to Minesweeper!\nPlease enter a difficulty (easy, medium, hard): "
  xg <- newStdGen
  yg <- newStdGen
  game <- createGameDiff (xg, yg)
  --display game
  putStrLn "Your board is ready to play!"
  putStrLn "to quit (and lose), enter \"quit\""
  endedGame <- play game
  putStrLn "Thanks for playing!"
  putStrLn "do you want to play again? y or yes for yes, anything else for no"
  again <- getLine
  let lagain = map toLower again
  if lagain == "y" || lagain == "yes"
  then main
  else putStrLn "Please come again soon! \n Leaving game..."

-- creates a game with a difficulty provided as input
createGameDiff gens = do
  difficulty <- getLine
  let ldifficulty = map toLower difficulty
  -- makes the game or resets main to get a propper difficulty
  if ldifficulty == "easy"
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
        display (Gamestate size bombs board winstate)
        putStrLn "What move would you like to make flag (\"f\") or click (\"c\")?"
        move <- getLine
        let lmove = map toLower move
        if lmove == "quit"
        then do
            putStrLn "quitting game..."
            return (Gamestate size bombs board Loss)
        else do
            if lmove == "f"
            then do
                updatedGame <- doFlag (Gamestate size bombs board winstate)
                play updatedGame
            else if lmove == "c"
            then do
                updatedGame <- doClick (Gamestate size bombs board winstate)
                play updatedGame
            else do
                putStrLn "Please enter a valid move: \"f\", \"c\", or \"quit\""
                play (Gamestate size bombs board winstate)
    

--doFlag will perform a flagging action on the location provided
doFlag game = do
  putStrLn "To flag, we need a location."
  loc <- getLoc game
  return (flagGame game loc)

--doClick will perform the click action on the location provided
doClick game = do
  putStrLn "To click we need a location."
  loc <- getLoc game
  return (clickGame game loc)

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
