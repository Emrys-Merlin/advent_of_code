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
toJoltage bank n = digitsToJoltage $ toJoltageDigits bank (replicate n 0)

toJoltageDigits :: Bank -> Digits -> [Int]
toJoltageDigits [] digits = digits
toJoltageDigits (battery:remaining_bank) digits =
  let new_digits = updateDigits battery digits
      rest_length = length remaining_bank
      digits_length = length new_digits
  in
    if digits_length > rest_length
     then
       let (digit:other_digits) = new_digits
       in digit:toJoltageDigits remaining_bank other_digits
     else
      toJoltageDigits remaining_bank new_digits

updateDigits :: Int -> Digits -> [Int]
updateDigits _ [] = []
updateDigits battery (digit:rest) =
  if battery > digit
  then battery:replicate (length rest) 0
  else
    digit:updateDigits battery rest


digitsToJoltage :: Digits -> Int
digitsToJoltage = foldl (\acc d -> acc * 10 + d) 0

toBanks :: [String] -> [[Int]]
toBanks = map toBank

toBank :: String -> [Int]
toBank "" = []
toBank (digit:rest) = digitToInt digit : toBank rest
