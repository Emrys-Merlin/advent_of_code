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
import Data.STRef
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
task02 content = show $ head x * head y
  where
    points = toPoints content
    unsorted_dists = toSDistIndex (zip points [0..])
    dists = sort unsorted_dists
    neighbors = map (\(_, i, j) -> (i, j)) dists
    (idx_x, idx_y) = edgeToFullyConnected (length points) neighbors
    x = points !! idx_x
    y = points !! idx_y

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
    startDFS :: STUArray s Int Bool -> Int -> ST s (Maybe Int)
    startDFS visited v = do
      isVisited <- readArray visited v
      if isVisited
      then return Nothing
      else do
        size <- dfs visited v
        return (Just size)

    dfs :: STUArray s Int Bool -> Int -> ST s Int
    dfs visited start = do
      isVisited <- readArray visited start
      if isVisited
      then return 0
      else do
        writeArray visited start True
        unvisited_neighbors <- filterM (fmap not . readArray visited) (adj_list ! start)
        sizes <- mapM (dfs visited) unvisited_neighbors
        return (1 + sum sizes)

data Node s = Node
  { value :: Int
  , parent :: STRef s (Maybe (Node s))
  , size :: STRef s Int
  } deriving (Eq)

edgeToFullyConnected :: Int -> [(Int, Int)] -> (Int, Int)
edgeToFullyConnected n edges = runST $ do
  node_lookup <- makeSet n
  maybe_edge <- foldM (step n node_lookup) Nothing edges
  return (fromMaybe (-1, -1) maybe_edge)

makeSet :: Int -> ST s (STArray s Int (Node s))
makeSet n = do
  arr <- newArray_ (0, n - 1)
  forM_ [0 .. n - 1] $ \i -> do
    pRef <- newSTRef Nothing
    sRef <- newSTRef 1
    writeArray arr i (Node i pRef sRef)
  return arr

findRoot :: Node s -> ST s (Node s)
findRoot node = do
  mp <- readSTRef (parent node)
  case mp of
    Nothing -> return node
    Just p -> do
      new_p <- findRoot p
      writeSTRef (parent node) (Just new_p)
      return new_p

merge :: STArray s Int (Node s) -> (Int, Int) -> ST s Int
merge node_lookup (i, j) = do
  ni <- readArray node_lookup i
  nj <- readArray node_lookup j
  node_i <- findRoot ni
  node_j <- findRoot nj
  if value node_i == value node_j
  then do
    readSTRef (size node_i)
  else do
    size_i <- readSTRef (size node_i)
    size_j <- readSTRef (size node_j)
    let new_total = size_i + size_j
    if size_i < size_j
    then do
      writeSTRef (parent node_i) (Just node_j)
      writeSTRef (size node_j) new_total
      return new_total
    else do
      writeSTRef (parent node_j) (Just node_i)
      writeSTRef (size node_i) new_total
      return new_total

step :: Int -> STArray s Int (Node s) -> Maybe (Int, Int) -> (Int, Int) -> ST s (Maybe (Int, Int))
step _ _ (Just e) _ = return (Just e)
step n node_lookup Nothing e = do
  size <- merge node_lookup e
  if size == n
  then return (Just e)
  else return Nothing
