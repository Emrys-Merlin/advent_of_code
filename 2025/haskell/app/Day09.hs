module Day09 (task01, task02) where

import Data.Maybe
import Utils
import qualified Data.Set as S

task01 :: String -> String
task01 content = show $ pointsToMaxRectangleArea points
  where ls = lines content
        points = map lineToPoint ls

task02 :: String -> String
task02 content = show (length ls) ++ "\t" ++ show (length rectangle_areas) ++ "\t" ++ show enclosing_rectangle ++ "\t" ++ show (rectangleArea (fromJust enclosing_rectangle))
  where
    ls = lines content
    points = map lineToPoint ls
    rectangle_areas = pointsToRectangleAreas points
    enclosing_rectangle = enclosingRectangle points


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

-- polygonArea :: [Point] -> Maybe Int
-- polygonArea [] = Nothing
-- polygonArea (p:points) =
--   let double_area = doublePolygonArea p (points ++ [p])
--   in
--     if even double_area
--     then Just (double_area `div` 2)
--     else Nothing

-- -- Implements Shoelace formula without factor 2
-- doublePolygonArea :: Point -> [Point] -> Int
-- doublePolygonArea _ [] = 0
-- doublePolygonArea (px, py) ((qx, qy):points) = (py + qy) * (px - qx) + doublePolygonArea (qx, qy) points

pointsToRectangleAreas :: [Point] -> [Int]
pointsToRectangleAreas [] = []
pointsToRectangleAreas (point:points) = pointsToRectangleAreasInner point points ++ pointsToRectangleAreas points

pointsToRectangleAreasInner :: Point -> [Point] -> [Int]
pointsToRectangleAreasInner _ [] = []
pointsToRectangleAreasInner p (q:points) = rectangleArea rectangle:pointsToRectangleAreasInner p points
  where rectangle = pointPairToRectangle p q

enclosingRectangle :: [Point] -> Maybe Rectangle
enclosingRectangle [] = Nothing
enclosingRectangle ((px, py):points) =
  case maybe_rectangle of
    Nothing -> Just Rectangle { x = (px, px + 1), y = (py, py + 1)}
    Just rectangle ->
      let (minx, maxx) = x rectangle
          (miny, maxy) = y rectangle
          new_minx = min minx px
          new_maxx = max maxx (px + 1)
          new_miny = min miny py
          new_maxy = max maxy (py + 1)
      in Just Rectangle {x = (new_minx, new_maxx), y = (new_miny, new_maxy)}
  where maybe_rectangle = enclosingRectangle points

boundaryPointList :: [Point] -> [Point]
boundaryPointList [] = []
boundaryPointList (p:points) = boundaryPointListInner p (points ++ [p])

boundaryPointListInner :: Point -> [Point] -> [Point]
boundaryPointListInner _ [] = []
boundaryPointListInner (px, py) ((qx, qy):points) =
  let line = if px == qx
             then [(px, ry) | ry <- [py..(qy-1)]]
             else [(rx, py) | rx <- [px..(qx-1)]]
  in line ++ boundaryPointListInner (qx, qy) points

boundaryPointSet :: [Point] -> S.Set Point
boundaryPointSet = S.fromList
