{-# LANGUAGE OverloadedStrings #-}
module Day10 (task01, task02) where

import Data.Scientific (Scientific)
import Data.List.Split
import Utils
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Data.Sequence (ViewL(..), (|>))
import Data.Bits
import Data.Maybe
import qualified Numeric.Optimization.MIP as MIP
import Numeric.Optimization.MIP ((.==.))
import Numeric.Optimization.MIP.Solver
import qualified Data.Text as T
import qualified Data.Map.Lazy as Map
-- import Debug.Trace

task01 :: String -> String
task01 content = show $ sum path_lengths
  where
    ls = lines content
    path_lengths = map shortestPath ls


task02 :: String -> IO String
task02 content = do
  let ls = lines content
  presses <- mapM solve2ForLine ls
  let total = sum presses
  return (show total)


shortestPath :: String -> Int
shortestPath line = fromJust path_length
  where
    (target, buttons) = parseLine line
    next = pressButtons buttons
    isTarget = (== target)
    path_length = shortestPathLength 0 isTarget next

parseLine :: String -> (Int, [Int])
parseLine line = (target, buttons)
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

parseLine2 :: String -> ([[Int]], [Int])
parseLine2 line = parseButtonsJoltages rest
  where
    (_, rest) = case words line of
      (w:ws) -> (w, ws)
      [] -> error "It should never happen: line is empty"

parseButtonsJoltages :: [String] -> ([[Int]], [Int])
parseButtonsJoltages [] = ([], [])
parseButtonsJoltages (group:rest) = (new_buttons, new_joltages)
  where
    (buttons, joltages) = parseButtonsJoltages rest
    is_button = head group == '('
    stripped_group = filter (\c -> c /= '{' && c/= '}' && c /= '(' && c /= ')') group
    button_or_joltages = map stringToInt (splitOn "," stripped_group)
    new_buttons = buttons ++ [button_or_joltages | is_button]
    new_joltages = if is_button then joltages else button_or_joltages

findShortestPressJoltage :: [[Int]] -> [Int] -> IO Int
findShortestPressJoltage buttons joltages_int = do
  let
    vars = map (MIP.varExpr . intToVar) [0..(n-1)]
    domains = map (\i -> (intToVar i, (MIP.IntegerVariable, (0, MIP.PosInf)))) [0..(n-1)]
    n = length buttons
    constraints = zipWith (buildConstraints vars buttons) joltages [0..]
    joltages = map fromIntegral joltages_int :: [Scientific]
    problem =
      MIP.def
      { MIP.objectiveFunction =
          MIP.def
          { MIP.objDir = MIP.OptMin
          , MIP.objExpr = sum vars
          }
      , MIP.constraints = constraints
      , MIP.varDomains = Map.fromList domains
      }
  solver <- solve cbc MIP.def{ solveTimeLimit = Just 10.0 } problem
  case MIP.solObjectiveValue solver of
    Just val -> return (round val)
    Nothing -> error "Solver could not find a solution (infeasible or timeout)"


intToVar :: Int -> MIP.Var
intToVar i = (MIP.Var . T.pack) $ "x" ++ show i

-- buildConstraints :: (Num c) => [MIP.Expr c] -> [[Int]] -> Int -> p -> MIP.Constraint c
buildConstraints :: (Num c, Foldable t, Eq c, Eq p) => [MIP.Expr c] -> [t p] -> c -> p -> MIP.Constraint c
buildConstraints vars buttons joltage i = sum filtered_vars .==. e_joltage
  where
    filtered_vars_buttons = filter (\(_, b) -> i `elem` b) (zip vars buttons)
    filtered_vars = map fst filtered_vars_buttons
    e_joltage = MIP.constExpr joltage


solve2ForLine :: String -> IO Int
solve2ForLine line = do
  let (buttons, joltages) = parseLine2 line
  findShortestPressJoltage buttons joltages
