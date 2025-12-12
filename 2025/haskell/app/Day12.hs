module Day12 (task01, task02) where

import Data.List.Split
import Utils
-- import Debug.Trace

data FitStatus
  = Fits
  | Undecided
  | DoesNotFit
  deriving (Eq, Ord, Show, Read, Bounded, Enum)


task01 :: String -> String
task01 content = "Fits: " ++ show fit ++ "\nUndecided: " ++ show undecided ++ "\nDoes not fit: " ++ show does_not_fit
  where
    (shapes, regions) = parseContent content
    fit_status = map (getFitStatus shapes) regions
    (fit, undecided, does_not_fit) = foldr countStatus (0, 0, 0) fit_status

    countStatus :: FitStatus -> (Int, Int, Int) -> (Int, Int, Int)
    countStatus Fits (f, u, dnf) = (f+1, u, dnf)
    countStatus Undecided (f, u, dnf) = (f, u+1, dnf)
    countStatus DoesNotFit (f, u, dnf) = (f, u, dnf+1)

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

fitByArea :: [Int] -> (Int, [Int]) -> Bool
fitByArea shapes (area, counts) = upper_bound <= area
  where
    upper_bound = sum $ zipWith (*) counts shapes

getFitStatus :: [Int] -> (Int, [Int]) -> FitStatus
getFitStatus shapes region
  | enough_by_area = Fits
  | too_little_by_area = DoesNotFit
  | otherwise = Undecided
  where
      enough_by_area = fitByArea (repeat 9) region
      too_little_by_area = not $ fitByArea shapes region
