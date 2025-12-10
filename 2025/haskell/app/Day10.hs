module Day10 (task01, task02) where

import Data.List.Split
import Utils
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Data.Sequence (ViewL(..), (|>))
import Data.Bits
import Data.Maybe
-- import Debug.Trace

task01 :: String -> String
task01 content = show $ sum path_lengths
  where
    ls = lines content
    path_lengths = map shortestPath ls


task02 :: String -> String
task02 content = "Apply task02 solution to content: " ++ take 100 content

shortestPath :: String -> Int
shortestPath line = fromJust path_length
  where
    (target, buttons, _) = parseLine line
    next = pressButtons buttons
    isTarget = (== target)
    path_length = shortestPathLength 0 isTarget next

parseLine :: String -> (Int, [Int], [Int])
parseLine line = (target, buttons, [])
  where
    (raw_target_str, rest) = case words line of
      (w:ws) -> (w, ws)
      [] -> error "It should never happen: line is empty"
    target_str = filter (\c -> c /= '[' && c /= ']') raw_target_str
    target = foldr (\c acc -> 2*acc + if c == '#' then 1 else 0) 0 target_str
    buttons = parseButtons rest

parseButtons :: [String] -> [Int]
parseButtons [] = []
parseButtons (candidate:rest) = new_buttons
  where
    buttons = parseButtons rest
    is_button = head candidate == '('
    stripped_candidate = filter (\c -> c /= '(' && c /=')' && c /= '{' && c /= '}') candidate
    int_list = map stringToInt (splitOn "," stripped_candidate)
    button = foldl (\acc n -> acc + 2^n) 0 int_list
    new_buttons = buttons ++ [button | is_button]

-- | Find the shortest distance from 'start' to a node satisfying 'isTarget'.
-- Returns 'Nothing' if no such node is reachable.
shortestPathLength :: (Ord a) => a -> (a -> Bool) -> (a -> [a]) -> Maybe Int
shortestPathLength start isTarget next = loop (Seq.singleton (start, 0)) (Set.singleton start)
  where
    loop queue visited =
      case Seq.viewl queue of
        Seq.EmptyL -> Nothing
        (curr, dist) :< rest ->
          if isTarget curr
            then Just dist
            else
              let neighbors = next curr
                  -- Filter neighbors that have not been seen yet
                  newNeighbors = filter (`Set.notMember` visited) neighbors

                  -- Add new neighbors to visited set immediately
                  newVisited = foldr Set.insert visited newNeighbors

                  -- Add new neighbors to queue with incremented distance
                  newQueue = foldl (\q n -> q |> (n, dist + 1)) rest newNeighbors
              in loop newQueue newVisited

pressButtons :: [Int] -> Int -> [Int]
pressButtons buttons state = map (`xor` state) buttons
