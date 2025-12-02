module Day02 (task01, task02) where

import Data.List
import Data.List.Split
import Data.Maybe
import Utils

task01 :: String -> String
task01 content =
  let ranges = splitOn "," content
  in show $ doublesInRanges ranges

task02 :: String -> String
task02 content =
  let ranges = splitOn "," content
  in show $ repeatsInRanges ranges

doublesInRanges :: [String] -> Int
doublesInRanges = foldr ((+) . doublesInRange) 0

doublesInRange :: String -> Int
doublesInRange range =
  let (left, right) = fromMaybe (error "Separator not found") (splitOnce range '-')
      start = stringToInt left
      stop = stringToInt right + 1 -- I want half-open intervals
  in countDoubles start stop

countDoubles :: Int -> Int -> Int
countDoubles start stop =
  if start == stop
  then 0
  else
    let new_count = if isDouble start then start else 0
    in new_count + countDoubles (start + 1) stop

isDouble :: Int -> Bool
isDouble candidate =
  let c = intToString candidate
      l = length c
  in (even l && isEvenDouble c)

isEvenDouble :: String -> Bool
isEvenDouble candidate =
  let (left, right) = splitAt middle candidate
      middle = length candidate `div` 2
  in left == right

repeatsInRanges :: [String] -> Int
repeatsInRanges [] = 0
repeatsInRanges (range:remaining_ranges) =
  let (left, right) = fromMaybe (error "Separator not found") (splitOnce range '-')
      start = stringToInt left
      stop = stringToInt right + 1
  in repeatsInRange start stop + repeatsInRanges remaining_ranges

repeatsInRange :: Int -> Int -> Int
repeatsInRange start stop =
  if start == stop
  then 0
  else repeatsInRange (start + 1) stop + if isRepeat (intToString start) then start else 0

isRepeat :: String -> Bool
isRepeat candidate =
  let l = length candidate
      divs = divisors l
  in isDivRepeat candidate divs

isDivRepeat :: String -> [Int] -> Bool
isDivRepeat _ [] = False
isDivRepeat candidate (divisor:remaining_divisors) = isNRepeat candidate divisor || isDivRepeat candidate remaining_divisors

isNRepeat :: String -> Int -> Bool
isNRepeat candidate n =
  case length candidate `compare` n of
    LT -> error "This should not happen"
    EQ -> True
    GT -> let (left, right) = splitAt n candidate
          in (left `isPrefixOf` right) && isNRepeat right n

-- Could be optimized to go up to sqrt
divisors :: Int -> [Int]
divisors n = [x | x <- [1..(n-1)], n `mod` x == 0]

