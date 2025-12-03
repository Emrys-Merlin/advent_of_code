module Day03 (task01, task02) where

import Data.Char
-- import Debug.Trace (trace)

type Bank = [Int]
type Digits = [Int]


task01 :: String -> String
task01 content = show $ sum $ map ( (`toJoltage` 2) . toBank) (lines content)

task02 :: String -> String
task02 content = show $ sum $ map ( (`toJoltage` 12) . toBank) (lines content)

toJoltage :: Bank -> Int -> Int
toJoltage bank n = digitsToJoltage $ toJoltageDigits bank n

toJoltageDigits :: Bank -> Int -> [Int]
toJoltageDigits _ 0 = []
toJoltageDigits bank n =
  let candidates = take (length bank - new_n) bank
      new_n = n - 1
      (digit, digit_idx) = idxMax candidates
      remaining_bank = drop (digit_idx + 1) bank
  in digit:toJoltageDigits remaining_bank new_n

idxMax :: Bank -> (Int, Int)
idxMax bank = foldl (\(max_battery, max_idx) (battery, curr_idx) -> if battery > max_battery then (battery, curr_idx) else (max_battery, max_idx)) (0, 0) (zip bank [0..])

digitsToJoltage :: Digits -> Int
digitsToJoltage = foldl (\acc d -> acc * 10 + d) 0

toBank :: String -> [Int]
toBank = map digitToInt
