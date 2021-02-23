-- a function that finds neighbors
module Neighbors (
      findNeighbors)
      where

type Location = (Int, Int)
type Size = (Int, Int)


-- Takes in a location and returns a list of neighbors that are within bounds
findNeighbors :: Location -> Size -> [Location]
findNeighbors location size = filter (isvalid size) (makeNeighbors location)
    where
      isvalid :: Location -> Location -> Bool
      isvalid (xbound, ybound) (x, y) = ((x >= 0) && (x < xbound)) && ((y >= 0) && (y < ybound))
      makeNeighbors :: Location -> [Location]
      makeNeighbors (x, y) = (x-1,y-1):(x,y-1):(x+1,y-1) : (x-1,y):(x+1,y) : (x-1,y+1):(x,y+1):(x+1,y+1):[]
