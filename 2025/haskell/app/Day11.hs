module Day11 (task01, task02) where

import Control.Monad.State
import Data.Maybe
import qualified Data.HashMap.Strict as HM
import Utils
-- import Debug.Trace

type Node = String
type Graph = HM.HashMap Node [Node]
type MemoTable = HM.HashMap Node Integer


task01 :: String -> String
task01 content = show $ traverseGraph graph "out" ["you"]
  where
    graph = parseContent content

-- Exploiting that the graph is acyclic
task02 :: String -> String
task02 content = show result
  where
    graph = parseContent content
    srv_to_fft = countPaths graph "fft" "svr"
    fft_to_dac = countPaths graph "dac" "fft"
    dac_to_out = countPaths graph "out" "dac"
    srv_to_dac = countPaths graph "dac" "svr"
    dac_to_fft = countPaths graph "fft" "dac"
    fft_to_out = countPaths graph "out" "fft"
    result = if fft_to_dac == 0
      then srv_to_dac * dac_to_fft * fft_to_out
      else srv_to_fft * fft_to_dac * dac_to_out

parseContent :: String -> Graph
parseContent content = HM.fromList entries
  where
    ls = lines content
    entries = map parseLine ls

parseLine :: String -> (Node, [Node])
parseLine line = (key, values)
  where
    (key, rest) = fromJust $ splitOnce line ':'
    values = filter (not . null) (words rest)

-- DFS with cycle detection. Rather inefficient, but good enough for part 1
traverseGraph :: Graph -> Node -> [Node] -> Int
traverseGraph _ _ [] = error "Cannot start with an empty path"
traverseGraph graph stop (node:rest) =
  let
    children = fromMaybe [] $ HM.lookup node graph
  in
    if node == stop
    then 1
    else
      if node `elem` rest -- found a cycle
      then 0
      else
        sum $ map (\c -> traverseGraph graph stop (c:node:rest)) children

-- Memoize counts
countPaths :: Graph -> Node -> Node -> Integer
countPaths graph stop start = evalState (go start) HM.empty
  where
    go :: Node -> State MemoTable Integer
    go node
      | node == stop = return 1
      | otherwise = do
          memo <- get
          case HM.lookup node memo of
            Just count -> return count
            Nothing -> do
              let neighbors = fromMaybe [] (HM.lookup node graph)
              counts <- mapM go neighbors
              let total = sum counts
              modify' (HM.insert node total)
              return total
