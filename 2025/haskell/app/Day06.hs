module Day06 (task01, task02) where

import Data.List.Split hiding (chunk)
import Utils
-- import Debug.Trace

task01 :: String -> String
task01 content =
  let ls = lines content
      (argument_lists, operators) = parseLines ls
  in show $ sumOperations argument_lists operators

task02 :: String -> String
task02 content =
  let ls = lines content
      (operators, interval_lengths) = parseOperators2 ls
      argument_lists = parseLines2 ls interval_lengths
  in show $ sumOperations argument_lists operators

parseLines :: [String] -> ([[String]], [Char])
parseLines [] = ([], [])
parseLines (line:rest) =
  let (argument_lists, operators) = parseLines rest
      arguments_or_operators = words line
      start = head arguments_or_operators
  in
    if start == "*" || start == "+"
    then (argument_lists, map head arguments_or_operators)
    else
      let new_argument_lists = if null argument_lists
                               then map (: []) arguments_or_operators
                               else zipWith (:) arguments_or_operators argument_lists
      in (new_argument_lists, operators)

sumOperations :: [[String]] -> [Char] -> Int
sumOperations argument_lists operations = sum $ applyOperations argument_lists operations

applyOperations :: [[String]] -> [Char] -> [Int]
applyOperations = zipWith applyOperation

applyOperation :: [String] -> Char -> Int
applyOperation arguments operator
  | operator == '+' = sum $ map stringToInt arguments
  | operator == '*' = product $ map stringToInt arguments
  | otherwise = error "This should not be reachable."

parseLines2 :: [String] -> [Int] -> [[String]]
parseLines2 [] _ = error "Should not be reached."
parseLines2 (line:rest) interval_lengths
  | head line == '+' || head line == '*' = map (\l -> replicate (l-1) "") interval_lengths
  | otherwise =
    let argument_lists = parseLines2 rest interval_lengths
    in parseLine2 line interval_lengths argument_lists

parseLine2 :: String -> [Int] -> [[String]] -> [[String]]
parseLine2 _ [] argument_lists = argument_lists
parseLine2 _ _ [] = error "Should not be reached."
parseLine2 line (l:interval_lengths) (arguments:argument_lists) =
  let (unstripped_chunk, rest) = splitAt l line
      chunk = take (l - 1) unstripped_chunk
      new_arguments = parseChunk chunk arguments
  in new_arguments:parseLine2 rest interval_lengths argument_lists

parseOperators2 :: [String] -> ([Char], [Int])
parseOperators2 [] = error "Should not be reached"
parseOperators2 (line:rest)
  | head line == '+' || head line == '*' = unzip $ map (\op -> (head op, length op)) (drop 1 $ split (keepDelimsL $ oneOf "*+") (line ++ " "))
  | otherwise = parseOperators2 rest

parseChunk :: String -> [String] -> [String]
parseChunk = zipWith parseNumber

parseNumber :: Char -> String -> String
parseNumber digit number
  | digit == ' ' = number
  | otherwise = digit:number
