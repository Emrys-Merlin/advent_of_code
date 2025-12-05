{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day05 (task01, task02) where

import Utils
import Data.Maybe
import Data.List

type Interval = (Int, Int)

task01 :: String -> String
task01 content = show $ length (filter (`inAnySortedDisjointInterval` merged_intervals) points)
  where ls = lines content
        (unsorted_intervals, points) = processLines ls False
        sorted_intervals = sortIntervals unsorted_intervals
        merged_intervals = mergeSortedIntervals sorted_intervals

task02 :: String -> String
task02 content = show $ sum $ map intervalLength merged_intervals
  where ls = lines content
        (unsorted_intervals, _) = processLines ls False
        sorted_intervals = sortIntervals unsorted_intervals
        merged_intervals = mergeSortedIntervals sorted_intervals

-- Optimize with binary search or sort points and remove intervals that are passed
inAnySortedDisjointInterval :: Int -> [Interval] -> Bool
inAnySortedDisjointInterval point = any (inside point)

processLines :: [String] -> Bool -> ([Interval], [Int])
processLines [] _ = ([], [])
processLines (line:ls) state
  | state = -- We are parsing points
    let (intervals, points) = processLines ls True
        point = stringToInt line
    in (intervals, point:points)
  | otherwise = -- We are still parsing intervals
    if null line
    then processLines ls True
    else
      let (intervals, points) = processLines ls False
          interval = toInterval line
      in (interval:intervals, points)

-- Transform to half open intervals
toInterval :: String -> Interval
toInterval s = (start, stop)
  where (left, right) = fromMaybe (error "Could not split.") (splitOnce s '-')
        start = stringToInt left
        stop = stringToInt right + 1

intervalLength :: Interval -> Int
intervalLength (start, stop) = stop - start

sortIntervals :: [Interval] -> [Interval]
sortIntervals = sort

data IntervalPoints = Below | Inside | Above
  deriving (Show, Eq)

relativeToInterval :: Int -> Interval -> IntervalPoints
relativeToInterval x (start, stop)
  | x < start = Below
  | x >= stop = Above
  | otherwise = Inside

isInside :: IntervalPoints -> Bool
isInside Inside = True
isInside _ = False

inside :: Int -> Interval -> Bool
inside x interval = isInside (relativeToInterval x interval)

mergeSortedIntervals :: [Interval] -> [Interval]
mergeSortedIntervals [] = []
mergeSortedIntervals (interval:intervals) = mergeSortedIntervalsInternal intervals interval

mergeSortedIntervalsInternal :: [Interval] -> Interval -> [Interval]
mergeSortedIntervalsInternal [] maybe_int = [maybe_int]
mergeSortedIntervalsInternal (next_interval:intervals) maybe_interval =
  let (left, right) = maybe_interval
      (_, next_right) = next_interval
  in case relativeToInterval right next_interval of
    Below -> maybe_interval:mergeSortedIntervalsInternal intervals next_interval
    Inside -> mergeSortedIntervalsInternal intervals (left, next_right)
    Above -> mergeSortedIntervalsInternal intervals maybe_interval
