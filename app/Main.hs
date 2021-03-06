module Main where

import MData
import MGeneration
import MInteraction
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
  putStrLn "Welcome to Minesweeper!\nPlease enter a difficulty (\"very easy\", \"easy\", \"medium\", \"hard\"): "
  xg <- newStdGen
  yg <- newStdGen
  game <- createGameDiff (xg, yg)
  --display game
  putStrLn "Your board is ready to play!"
  putStrLn "to quit (and lose), enter \"quit\"\n"
  endedGame <- play game
  putStrLn "Thanks for playing!"
  putStrLn "do you want to play again? \"y\" or \"yes\" for yes, anything else for no"
  again <- getLine
  let lagain = map toLower again
  if lagain == "y" || lagain == "yes"
  then main
  else putStrLn "Please come again soon! \nLeaving game..."

-- creates a game with a difficulty provided as input
createGameDiff :: (StdGen, StdGen) -> IO Game
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
    putStrLn "Please enter a valid difficulty, one of \"very easy\", \"easy\", \"medium\", or \"hard\": "
    createGameDiff gens

--play function takes a game and will check the win (and lose) conditon on every call
-- if the game has been won or lost, return
play :: Game -> IO Game
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
        putStrLn "\nWhat move would you like to do:\n make flag (\"f\"), click (\"c\"), receive a hint (\"h\"), or have the game be solved for you (\"s\")? (or \"quit\")"
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
            else if lmove == "s"
            then do
                (solveGame, locs) <- doSolve (Gamestate size bombs board winstate)
                display solveGame
                putStrLn ("Your game was solved in " ++ (show (length locs)) ++ " moves by doing the following moves (first to last): ")
                putStrLn (show (convertLoc locs))
                return solveGame
            else do
                putStrLn "Please enter a valid move: \"f\", \"c\", \"h\", \"s\" or \"quit\""
                play (Gamestate size bombs board winstate)

-- Play Helpers

-- does the solve returning the pair of game and locations clicked
doSolve :: Game -> IO (Game, [Location])
doSolve game = do
  putStrLn "Solving Game...\nRemoving invalid flags..\nClicking cells...\n ...Done!"
  return (bestSolver game [])

-- performs the assistant action
doHint :: Game -> IO Game
doHint game = do
  putStrLn "\nWould you like to have a bomb flagged for you (\"f\"), have a safe spot clicked for you (\"c\"), or remove invalid flags (\"r\")? , or \"no\" to leave assistant"
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
  else if lhint == "r"
  then do doRemove game
  else if lhint == "no"
  then do return game
  else do
    putStrLn "Please enter a valid hint request, flag a bomb (\"f\") or click a safe spot(\"c\"), or \"no\" to leave assistant"
    doHint game

--flagBombLocation will flag a bomb on the board for the user (currently flags only one bomb)
flagBombLocation :: Game -> IO Game
flagBombLocation (Gamestate size bombs board winstate) = do
  putStrLn "\nFlagged a bomb for you!!"
  return (assistFlag (Gamestate size bombs board winstate))

--assistClickLocation will click a safe place on the board for the user
assistClickLocation :: Game -> IO Game
assistClickLocation game = do
  putStrLn "do you want the first available spot(\"f\") or the best location (\"b\")"
  typecl <- getLine
  let ltypel = map toLower typecl
  if ltypel == "b"
  then do 
    putStrLn "\nClicked the best spot!\n"
    return (clickBest game)
  else if ltypel == "f"
  then do
    putStrLn "\nClicked a safe spot for you!!\n"
    return (assistClick game)
  else do
    putStrLn "Please enter a valid type of click: \"f\" or \"b\""
    assistClickLocation game

--performs the removal of invalid flags on the given game
doRemove :: Game -> IO Game
doRemove game = do
  putStrLn "removed all your invalid flags"
  return (removeInvalidGame game)

--doFlag will perform a flagging action on the location provided
doFlag :: Game -> IO Game
doFlag (Gamestate size bombs board winstate) = do
  putStrLn "\nTo flag, we need a location."
  loc <- getLoc (Gamestate size bombs board winstate)
  flagMsg board loc
  return (flagGame (Gamestate size bombs board winstate) loc)

--doClick will perform the click action on the location provided
doClick :: Game -> IO Game
doClick (Gamestate size bombs board winstate) = do
  putStrLn "\nTo click we need a location."
  loc <- getLoc (Gamestate size bombs board winstate)
  clickMsg board loc
  -- save board in case it's going to lose and prints out best win condition
  if ((getContent board loc) == Bomb) && ((getState board loc) == Covered)
    then do 
      whatToDo (Gamestate size bombs board winstate) 
    else putStrLn ""
  return (clickGame (Gamestate size bombs board winstate) loc)

--getLoc helper for doFlag and doClick that gets a valid location from the user given a certain game
getLoc :: Game -> IO (Int, Int)
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
validx coord (Gamestate (xsz, ysz) bombs board winstate) =
  if (length coord == 1) && ((coord!!0) `elem` (take (xsz) ['a'..'z']))
  then True
  else False

validy:: String -> Game -> Bool
validy coord (Gamestate (xsz, ysz) bombs board winstate) =
  if (length coord == 1) && ((coord!!0) `elem` (take (ysz) ['a'..'z']))
  then True
  else False

--convert takes a string letter and converts to the correct int coordination representation
convert:: String -> Int
convert coord = fromJust $ elemIndex (coord!!0) ['a'..'z']

--converts a num to a letter representation
convertnum :: Int -> Char
convertnum n = ['a'..'z'] !! n

--converts a list of locaitons to a list of letter coordinates
convertLoc :: [(Int, Int)] -> [(Char, Char)]
convertLoc locs = map (\ (a,b) -> ((convertnum a), (convertnum b))) locs

--what to do will return a soved version of the board that tells the player how they could have won the game
whatToDo :: Game -> IO ()
whatToDo game = do
  (solveGame, locs) <- doSolve game
  putStrLn "You could have won by doing the following move sequence (first to last): "
  putStrLn (show (convertLoc locs))
  putStrLn "...to end at this board: "
  display solveGame
  putStrLn "... but instead you ended here: "


--determines what message to send after a move (if flag, bomb, or already revealed)
clickMsg :: Board -> Location -> IO ()
clickMsg board loc = do
  if (getState board loc) == Uncovered
  then do revealMsg
  else do
    if (getState board loc) == Flagged
    then do clickFlagMsg
    else do
      if (getContent board loc) == Bomb
      then do bombMsg
      else do putStrLn ("You found a valid space, good job!\nYou have revealed " ++ (show (countRevealedCells board)) ++ " now \n")

--prints a message saying location is flagged
clickFlagMsg :: IO ()
clickFlagMsg = do
  putStrLn "This cell is flagged, please unflag before clicking\n"

--prints a message indicating a bomb was pressed
bombMsg :: IO ()
bombMsg = do
  putStrLn "You pressed a bomb...\n"

--prints a message stating that the locaiton is already revealed
revealMsg :: IO ()
revealMsg = do
  putStrLn "This cell is already revealed, try another one\n"

--prints a message saying if the cell was flagged, unflagged, or could not be flagged
flagMsg :: Board -> Location -> IO ()
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
