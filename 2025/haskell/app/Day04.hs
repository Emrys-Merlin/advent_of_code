module Day04 (task01, task02) where

import qualified Data.Set as Set

type Coord = (Int, Int)

task01 :: String -> String
task01 content =
  let coord_set = Set.fromDistinctAscList $ extractCoords content
  in show $ accessiblePaperRole coord_set

task02 :: String -> String
task02 content =
  let coord_set = Set.fromDistinctAscList (extractCoords content)
  in show $ recursiveAccessiblePaperRole coord_set

accessiblePaperRole :: Set.Set Coord -> Int
accessiblePaperRole coord_set = Set.size $ Set.filter (`isAccessible` coord_set) coord_set

recursiveAccessiblePaperRole :: Set.Set Coord -> Int
recursiveAccessiblePaperRole coord_set =
  let new_coord_set = Set.filter (`isNotAccessible` coord_set) coord_set
      accessible = Set.size coord_set - Set.size new_coord_set
  in accessible + if accessible /= 0
     then recursiveAccessiblePaperRole new_coord_set
     else 0

isNotAccessible :: Coord -> Set.Set Coord -> Bool
isNotAccessible coord coords = not $ isAccessible coord coords

isAccessible :: Coord -> Set.Set Coord -> Bool
isAccessible coord coords = length filled_neighbors < 4
  where neighbors = get8Neighbors coord
        filled_neighbors = [x | x <- neighbors, Set.member x coords]

extractCoords :: String -> [Coord]
extractCoords content =
  let rows = zip (lines content) [0..]
  in [(i, j) | (row, i) <- rows, (c, j) <- zip row [0..], c == '@']

get8Neighbors :: Coord -> [Coord]
get8Neighbors (i, j) = [(i + di, j + dj) | di <- [-1, 0, 1], dj <- [-1, 0, 1], di /= 0 || dj /= 0]
