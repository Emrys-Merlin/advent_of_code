module Day08 (task01, task02) where

import Data.Array
import Utils
import Data.List.Split hiding (chunk)
import Data.Array.ST
import Control.Monad
import Control.Monad.ST
import Data.Maybe
import Data.List
import Data.Ord
-- import Debug.Trace

type Point = [Int]  -- Think about enforcing length 3

task01 :: String -> String
task01 content = show $ product (take 3 sizes)
  where
    points = toPoints content
    unsorted_dists = toSDistIndex (zip points [0..])
    dists = sort unsorted_dists
    neighbors = take 1000 (map (\(_, i, j) -> (i, j)) dists)
    adj_list = buildAdjacencyList (length points) neighbors
    sizes = sortBy (comparing Data.Ord.Down) (connectedComponentSizes adj_list)


task02 :: String -> String
task02 content = "Apply task02 solution to content: " ++ take 100 content


toPoint :: String -> Point
toPoint line = map stringToInt (splitOn "," line)

toPoints :: String -> [Point]
toPoints content = map toPoint ls
  where ls = lines content

squaredDist :: Point -> Point -> Int
squaredDist x y = sum $ zipWith (\xn yn -> (xn - yn)*(xn - yn)) x y

toSDistIndex :: [(Point, Int)] -> [(Int, Int, Int)]
toSDistIndex [] = []
toSDistIndex (point:points) = toSDistIndexInner point points ++ toSDistIndex points

toSDistIndexInner :: (Point, Int) -> [(Point, Int)] -> [(Int, Int, Int)]
toSDistIndexInner _ [] = []
toSDistIndexInner (x, i) ((y, j):points) = (squaredDist x y, i, j):toSDistIndexInner (x, i) points

buildAdjacencyList :: Int -> [(Int, Int)] -> Array Int [Int]
buildAdjacencyList n edges = runSTArray $ do
  arr <- newArray (0, n-1) []
  forM_ edges $ \(i, j) -> do
    neighbors <- readArray arr i
    writeArray arr i (j : neighbors)
    neighbors_j <- readArray arr j
    writeArray arr j (i : neighbors_j)
  return arr

connectedComponentSizes :: Array Int [Int] -> [Int]
connectedComponentSizes adj_list = runST $ do
  let (lo, hi) = bounds adj_list
      vertices = [lo..hi]
  visited <- newArray (lo, hi) False :: ST s (STUArray s Int Bool)
  sizes <- mapM (startDFS visited) vertices
  return $ catMaybes sizes
  where
    startDFS visited v = do
      isVisited <- readArray visited v
      if isVisited
      then return Nothing
      else do
        size <- dfs visited v
        return (Just size)

    dfs visited start = do
      isVisited <- readArray visited start
      if isVisited
      then return 0
      else do
        writeArray visited start True
        unvisited_neighbors <- filterM (fmap not . readArray visited) (adj_list ! start)
        sizes <- mapM (dfs visited) unvisited_neighbors
        return (1 + sum sizes)
