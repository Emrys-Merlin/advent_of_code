module Day01 (task01, task02) where

import Utils

task01 :: String -> String
task01 content =
  let lineList = lines content
      count = zeroCount lineList 50 0
  in show count

task02 :: String -> String
task02 content =
  let lineList = lines content
      count = zeroCountOver lineList 50 0
  in show count

zeroCount :: [String] -> Int -> Int -> Int
zeroCount [] _ count = count
zeroCount (op:new_lines) position count =
  let new_position = updatePosition op position
      new_count = count + if new_position == 0 then 1 else 0
  in zeroCount new_lines new_position new_count

zeroCountOver :: [String] -> Int -> Int -> Int
zeroCountOver [] _ count = count
zeroCountOver (op:new_lines) position count =
  let (cycles, remainder) = extractCyclesRemainder op
      signed_new_position = position + remainder
      {-
        If remainder is zero, we did not mov -> no click
        If position is zero, we counted last time and cannot cross another boundary
        In the remaining cases, we need to check for out of bounds
      -}
      extra_count = if (remainder /= 0) && position /= 0 && ((signed_new_position <= 0) || (signed_new_position >= 100))
                    then 1
                    else 0
      new_position = signed_new_position `mod` 100
      new_count = count + cycles + extra_count
  in zeroCountOver new_lines new_position new_count

updatePosition :: String -> Int -> Int
updatePosition [] _ = error "Empty string"
updatePosition (direction:offset_str) position =
  let offset = stringToInt offset_str
      sign = if direction == 'R' then 1 else -1
  in (position + sign*offset) `mod` 100

extractCyclesRemainder :: String -> (Int, Int)
extractCyclesRemainder [] = error "Empty string"
extractCyclesRemainder (direction:offset_str) =
  let offset = stringToInt offset_str
      sign = if direction == 'R' then 1 else -1
      signed_offset = sign*offset
      cycles = offset `div` 100
      remainder = signed_offset `mod` 100
      -- Need to exclude remainder == -100 in the negative sign case
      signed_remainder = if (sign > 0) || (remainder == 0) then remainder else remainder - 100
  in (cycles, signed_remainder)

