{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Day03 (task01, task02) where

import Data.Char
-- import Debug.Trace (trace)

type Bank = [Int]
type Digits = [Int]


task01 :: String -> String
task01 content =
  let lineList = lines content
      banks = toBanks lineList
  in show $ totalJoltage banks 2

task02 :: String -> String
task02 content =
  let lineList = lines content
      banks = toBanks lineList
  in show $ totalJoltage banks 12

totalJoltage :: [Bank] -> Int -> Int
totalJoltage banks n = sum (map (`toJoltage` n) banks)

toJoltage :: Bank -> Int -> Int
toJoltage bank n = digitsToJoltage $ toJoltageDigits bank n

toJoltageDigits :: Bank -> Int -> [Int]
toJoltageDigits _ 0 = []
toJoltageDigits bank n =
  let candidates = take (length bank - new_n) bank
      new_n = n - 1
      digit_idx = idxMax candidates
      digit = bank !! digit_idx
      remaining_bank = drop (digit_idx + 1) bank
  in digit:toJoltageDigits remaining_bank new_n

idxMax :: Bank -> Int
idxMax bank = idx
  where (_, idx) = foldl (\(max_battery, max_idx) (battery, curr_idx) -> if battery > max_battery then (battery, curr_idx) else (max_battery, max_idx)) (0, 0) (zip bank [0..])

digitsToJoltage :: Digits -> Int
digitsToJoltage = foldl (\acc d -> acc * 10 + d) 0

toBanks :: [String] -> [[Int]]
toBanks = map toBank

toBank :: String -> [Int]
toBank "" = []
toBank (digit:rest) = digitToInt digit : toBank rest
