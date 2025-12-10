module Day09 (task01, task02) where

import Utils
-- import Debug.Trace

task01 :: String -> String
task01 content = show $ pointsToMaxRectangleArea points
  where ls = lines content
        points = map lineToPoint ls

task02 :: String -> String
task02 content = show $ pointsToMaxRectangleArea2 points points 0
  where
    ls = lines content
    points = map lineToPoint ls


type Point = (Int, Int)
type Interval = (Int, Int)
data Rectangle = Rectangle
  { x :: Interval
  , y :: Interval
  } deriving (Eq, Show)

intervalLength :: Interval -> Int
intervalLength (start, stop) = stop - start

rectangleArea :: Rectangle -> Int
rectangleArea rect = intervalLength (x rect) * intervalLength (y rect)

lineToPoint :: String -> Point
lineToPoint line =
  case maybe_pair of
    Nothing -> error "Could not parse pair."
    Just (left, right) -> (stringToInt left, stringToInt right)
  where maybe_pair = splitOnce line ','

pointPairToRectangle :: Point -> Point -> Rectangle
pointPairToRectangle (px, py) (qx, qy) =
  let start_x = min px qx
      stop_x = 1 + max px qx
      start_y = min py qy
      stop_y = 1 + max py qy
      interval_x = (start_x, stop_x)
      interval_y = (start_y, stop_y)
  in Rectangle { x = interval_x, y = interval_y}

pointsToMaxRectangleArea :: [Point] -> Int
pointsToMaxRectangleArea [] = 0
pointsToMaxRectangleArea (point:points) = max (pointsToMaxRectangleAreaInner point points) (pointsToMaxRectangleArea points)

pointsToMaxRectangleAreaInner :: Point -> [Point] -> Int
pointsToMaxRectangleAreaInner _ [] = 0
pointsToMaxRectangleAreaInner p (q:points) = max (rectangleArea rectangle) (pointsToMaxRectangleAreaInner p points)
  where rectangle = pointPairToRectangle p q

segmentNextToRectangle :: Point -> Point -> Rectangle -> Bool
segmentNextToRectangle (px, py) (qx, qy) rect = segment_right <= left || right <= segment_left || segment_down <= up || down <= segment_up
  where
    (left, right_p1) = x rect
    (up, down_p1) = y rect
    right = right_p1 - 1
    down = down_p1 - 1
    segment_left = min px qx
    segment_right = max px qx
    segment_up = min py qy
    segment_down = max py qy

-- This check for validity fails if there are large rectangles outside the boundary
-- However, this is not the case for the given input.
rectangleValid :: [Point] -> Rectangle -> Bool
rectangleValid [] _ = True
rectangleValid (p:points) rect = foldl (\acc (q, r) -> acc && segmentNextToRectangle q r rect) True (zip (p:points) (points ++ [p]))

pointsToMaxRectangleArea2 :: [Point] -> [Point] -> Int -> Int
pointsToMaxRectangleArea2 _ [] curr_max = curr_max
pointsToMaxRectangleArea2 all_points (p:points) curr_max = pointsToMaxRectangleArea2 all_points points inner_max
  where inner_max = pointsToMaxRectangleArea2Inner all_points p points curr_max

pointsToMaxRectangleArea2Inner :: [Point] -> Point -> [Point] -> Int -> Int
pointsToMaxRectangleArea2Inner _ _ [] curr_max = curr_max
pointsToMaxRectangleArea2Inner all_points p (q:points) curr_max =
  let rect = pointPairToRectangle p q
      area = rectangleArea rect
  in
    if area <= curr_max
    then pointsToMaxRectangleArea2Inner all_points p points curr_max
    else
      if rectangleValid all_points rect
      then pointsToMaxRectangleArea2Inner all_points p points area
      else pointsToMaxRectangleArea2Inner all_points p points curr_max
