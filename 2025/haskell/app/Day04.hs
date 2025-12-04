module Day04 (task01, task02) where

import qualified Data.Set as Set

type Coord = (Int, Int)

task01 :: String -> String
task01 content =
  let coord_list = extractCoords content
  in show $ accessiblePaperRole coord_list

task02 :: String -> String
task02 content =
  let coord_list = extractCoords content
  in show $ recursiveAccessiblePaperRole coord_list

accessiblePaperRole :: [Coord] -> Int
accessiblePaperRole coord_list =
  let coord_set = Set.fromList coord_list
  in sum $ map (\x -> if isAccessible x coord_set then 1 else 0) coord_list

recursiveAccessiblePaperRole :: [Coord] -> Int
recursiveAccessiblePaperRole coord_list =
  let coord_set = Set.fromList coord_list
      new_coord_list = [x | x <- coord_list, not (isAccessible x coord_set)]
      accessible = length coord_list - length new_coord_list
  in accessible + if accessible /= 0
    then recursiveAccessiblePaperRole new_coord_list
    else 0

isAccessible :: Coord -> Set.Set Coord -> Bool
isAccessible coord coords = length (Set.intersection neighbors coords) < 4
  where neighbors = get8Neighbors coord

extractCoords :: String -> [Coord]
extractCoords content =
  let rows = zip (lines content) [0..]
  in [(i, j) | (row, i) <- rows, (c, j) <- zip row [0..], c == '@']

get8Neighbors :: Coord -> Set.Set Coord
get8Neighbors (i, j) = Set.fromList [(i + di, j + dj) | di <- [-1, 0, 1], dj <- [-1, 0, 1], di /= 0 || dj /= 0]
