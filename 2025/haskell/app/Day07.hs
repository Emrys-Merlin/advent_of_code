module Day07 (task01, task02) where

import Data.List
import Data.Maybe

task01 :: String -> String
task01 content =
  let grid = toGrid content
      start = findStart (head grid)
  in show $ splitBeams grid [start]

task02 :: String -> String
task02 content =
  let grid = toGrid content
  in case grid of
    [] -> error "Empty grid."
    (first_row:remaining_grid) ->
      let start = findStart first_row
          l = length first_row
          counts = replicate start 0 ++ [1] ++ replicate (l - start - 1) 0
      in show $ beamCount remaining_grid counts

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

-- TODO: deduplicae entries directly
split :: [Char] -> [Int] -> ([Int], Int)
split _ [] = ([], 0)
split row (beam:remaining_beams) =
  let c = row !! beam
      (added_beams, counter)
        | c == '^' = ([beam - 1, beam + 1], 1)
        | otherwise = ([beam], 0)
      (other_beams, other_counter) = split row remaining_beams
  in (added_beams ++ other_beams, counter + other_counter)

beamCount :: [[Char]] -> [Int] -> Int
beamCount [] counts = sum counts
beamCount (row:grid) counts =
  let
    prev = ('.', 0)
    rows_counts = zip (row ++ ".") (counts ++ [0])
  in
    case rows_counts of
      [] -> error "This should not be reached."
      curr:rest ->
        let
          new_counts = iterRow prev curr rest
        in beamCount grid new_counts

iterRow :: (Char, Int) -> (Char, Int) -> [(Char, Int)] -> [Int]
iterRow _ _ [] = []
iterRow (prev_char, prev_count) (curr_char, curr_count) ((next_char, next_count):rows_counts) = new_count:iterRow (curr_char, curr_count) (next_char, next_count) rows_counts
  where
    new_count =
      if curr_char == '^'
      then 0
      else curr_count + (if prev_char == '^' then prev_count else 0) + (if next_char == '^' then next_count else 0)
