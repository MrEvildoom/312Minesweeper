-- functions for displaying Minesweeper in a text interface

module MDisplay where

import MData

{--
Displays, when printed, look like this

  00 Bombs
  0 1 2 3 4
0 X X F 1 0
1 X X 2 0 0
2 X F 1 1 1
3 X X X X X
4 X X X X X

input "(x, y, c)"
   or "(x, y, r)"

X is a covered square
numbers are clues
F is a flag
--}

display :: Game -> IO ()
display g = putStrLn (makeInfoLine g)

makeInfoLine :: Game -> [Char]
makeInfoLine (Gamestate _ bombs _) = (show bombs) ++ " Bombs \n"
-- makeBoard :: Game -> [Char]
