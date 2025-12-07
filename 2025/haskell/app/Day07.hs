module Day07 (task01, task02) where

import Data.List
import Data.Maybe

task01 :: String -> String
task01 content =
  let grid = toGrid content
      start = findStart (head grid)
  in show $ splitBeams grid [start]

task02 :: String -> String
task02 content = "Apply task02 solution to content: " ++ take 100 content

toGrid :: String -> [[Char]]
toGrid = lines

findStart :: [Char] -> Int
findStart row =
  let maybe_start = elemIndex 'S' row
  in fromJust maybe_start

splitBeams :: [[Char]] -> [Int] -> Int
splitBeams [] _ = 0
splitBeams (row:rest) beams =
  let (duplicated_beams, counter) = split row beams
      new_beams = nub duplicated_beams
  in counter + splitBeams rest new_beams

split :: [Char] -> [Int] -> ([Int], Int)
split _ [] = ([], 0)
split row (beam:remaining_beams) =
  let c = row !! beam
      (added_beams, counter)
        | c == '^' = ([beam - 1, beam + 1], 1)
        | otherwise = ([beam], 0)
      (other_beams, other_counter) = split row remaining_beams
  in (added_beams ++ other_beams, counter + other_counter)
