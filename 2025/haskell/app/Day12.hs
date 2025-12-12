module Day12 (task01, task02) where

import Data.List.Split
import Utils
-- import Debug.Trace

task01 :: String -> String
task01 content = show count
  where
    (_, regions) = parseContent content
    count :: Int
    count = sum $ map (fromEnum . heuristicFit) regions

task02 :: String -> String
task02 _ = "Freebie"


parseContent :: String -> ([Int], [(Int, [Int])])
parseContent content = (shapes, regions)
  where
    blocks = splitOn "\n\n" content
    (region_block, rest) = case reverse blocks of
      (rb:r) -> (rb, r)
      [] -> error "Too few blocks"
    regions = parseRegionBlock region_block
    shapes = reverse $ map parseShapeBlock rest

parseRegionBlock :: String -> [(Int, [Int])]
parseRegionBlock block = map parseRegionLine ls
  where ls = lines block

-- Return region area instead of full shape
parseRegionLine :: String -> (Int, [Int])
parseRegionLine line = (area, counts)
  where
    (left, right) = case splitOnce line ':' of
      Nothing -> error "Could not find ':'"
      Just res -> res
    (l, r) = case splitOnce left 'x' of
      Nothing -> error "Could not find 'x'"
      Just res -> res
    area = stringToInt l * stringToInt r
    counts = map stringToInt (words right)

-- Only returns the area of the shape
parseShapeBlock :: String -> Int
parseShapeBlock block = sum $ map (\c -> fromEnum (c == '#')) block

-- Check if fully filled 3x3 shapes fit
heuristicFit :: (Int, [Int]) -> Bool
heuristicFit (area, counts) = upper_bound <= area
  where
    upper_bound = 9 * sum counts
