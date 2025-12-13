-- Rewrite of my solution based on
-- https://old.reddit.com/r/adventofcode/comments/1pk87hl/2025_day_10_part_2_bifurcate_your_way_to_victory/
module Day10 (task01, task02) where

import Data.List
import Data.List.Split
import Utils
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Data.Sequence (ViewL(..), (|>))
import Data.Bits
import Data.Maybe
import qualified Data.HashMap.Strict as HM
import Control.Monad.State
-- import Debug.Trace

type Parity = Int -- binary encoded
type ActivationPattern = Int -- binary encoded
type DecodedButton = [Int]
type Button = Int -- binary encoded
type Buttons = [Button]
type Joltages = [Int]
type JoltagesPressMemo = HM.HashMap Joltages Int -- Memoization of Joltages -> min button press
type ParityToActivation = HM.HashMap Parity [ActivationPattern] -- Lookup table for button press combinations to get parity


task01 :: String -> String
task01 content = show $ sum path_lengths
  where
    ls = lines content
    path_lengths = map shortestPath ls


task02 :: String -> String
task02 content = show total
  where
    ls = lines content
    total = sum $ map applyPressCountToLine ls

shortestPath :: String -> Int
shortestPath line = fromJust path_length
  where
    (target, buttons) = parseLine line
    next = pressButtons buttons
    isTarget = (== target)
    path_length = shortestPathLength 0 isTarget next

parseLine :: String -> (Int, [Int])
parseLine line = (target, buttons)
  where
    (raw_target_str, rest) = case words line of
      (w:ws) -> (w, ws)
      [] -> error "It should never happen: line is empty"
    target_str = filter (\c -> c /= '[' && c /= ']') raw_target_str
    target = foldr (\c acc -> 2*acc + if c == '#' then 1 else 0) 0 target_str
    buttons = parseButtons rest

parseButtons :: [String] -> [Int]
parseButtons [] = []
parseButtons (candidate:rest) = new_buttons
  where
    buttons = parseButtons rest
    is_button = head candidate == '('
    stripped_candidate = filter (\c -> c /= '(' && c /=')' && c /= '{' && c /= '}') candidate
    int_list = map stringToInt (splitOn "," stripped_candidate)
    button = decodedButtonToBinary int_list
    new_buttons = buttons ++ [button | is_button]

decodedButtonToBinary :: DecodedButton -> Button
decodedButtonToBinary = foldl' (\acc n -> acc + 2^n) 0

-- | Find the shortest distance from 'start' to a node satisfying 'isTarget'.
-- Returns 'Nothing' if no such node is reachable.
shortestPathLength :: (Ord a) => a -> (a -> Bool) -> (a -> [a]) -> Maybe Int
shortestPathLength start isTarget next = loop (Seq.singleton (start, 0)) (Set.singleton start)
  where
    loop queue visited =
      case Seq.viewl queue of
        Seq.EmptyL -> Nothing
        (curr, dist) :< rest ->
          if isTarget curr
            then Just dist
            else
              let neighbors = next curr
                  -- Filter neighbors that have not been seen yet
                  newNeighbors = filter (`Set.notMember` visited) neighbors

                  -- Add new neighbors to visited set immediately
                  newVisited = foldr Set.insert visited newNeighbors

                  -- Add new neighbors to queue with incremented distance
                  newQueue = foldl (\q n -> q |> (n, dist + 1)) rest newNeighbors
              in loop newQueue newVisited

pressButtons :: Buttons -> Parity -> [Parity]
pressButtons buttons parity = map (`xor` parity) buttons

parseButtonsJoltages :: [String] -> ([DecodedButton], Joltages)
parseButtonsJoltages [] = ([], [])
parseButtonsJoltages (grp:rest) = (new_buttons, new_joltages)
  where
    (buttons, joltages) = parseButtonsJoltages rest
    is_button = head grp == '('
    stripped_group = filter (\c -> c /= '{' && c/= '}' && c /= '(' && c /= ')') grp
    button_or_joltages = map stringToInt (splitOn "," stripped_group)
    new_buttons = buttons ++ [button_or_joltages | is_button]
    new_joltages = if is_button then joltages else button_or_joltages

pressesForJoltages :: Buttons -> Joltages -> Int
pressesForJoltages buttons joltages = evalState (pressCount joltages) HM.empty
  where
    parityToActivation = buildParityToActivation buttons

    pressCount :: Joltages -> State JoltagesPressMemo Int
    pressCount jolts
      | all (== 0) jolts = return 0
      | otherwise = do
        memo <- get
        case HM.lookup jolts memo of
          Just press_count -> return press_count
          Nothing -> do
            let
              joltage_parity = toParity jolts
              activationPatterns = fromMaybe [] (HM.lookup joltage_parity parityToActivation)
              potential_joltages = map (decreaseJoltagesByActivation jolts buttons) activationPatterns
              list_of_filtered_joltages_count_actives = filter (\(pj, _) -> validJoltages pj) (zip potential_joltages count_active)
              (new_list_of_joltages, new_count_active) = unzip list_of_filtered_joltages_count_actives
              halfed_list_of_joltages = map (map (`div` 2)) new_list_of_joltages
              count_active = map countActive activationPatterns
            half_presses <- mapM pressCount halfed_list_of_joltages
            let
              total_presses = zipWith (\ hp ca -> 2*hp + ca) half_presses new_count_active
              min_presses = if null total_presses
                then 1000000000 -- No valid joltage states found, return "infinity"
                else minimum total_presses
            modify' (HM.insert jolts min_presses)
            return min_presses


toParity :: Joltages -> Int
toParity = foldr (\n acc -> 2*acc + (n `mod` 2)) 0

activationPatternToParity :: Buttons -> ActivationPattern -> Parity
activationPatternToParity buttons combination =
  foldl' (\acc (button, i) -> if testBit combination i then acc `xor` button else acc) 0 (zip buttons [0..])


validJoltages :: Joltages -> Bool
validJoltages joltages = non_negative && all_even
  where
    non_negative = foldl' (\acc jolt -> acc && (jolt >= 0)) True joltages
    all_even = toParity joltages == 0

buildParityToActivation :: Buttons -> ParityToActivation
buildParityToActivation buttons = HM.fromListWith (++) parity_activation
  where
    n = length buttons
    activations = [0..(2^n - 1)] -- I need to start from zero, in case the parity is even already to just divide -- I need to start from zero, in case the parity is even already to just divide
    parity_activation = map (\activation -> (activationPatternToParity buttons activation, [activation])) activations

filterButtonsByActivation :: ActivationPattern -> Buttons -> Buttons
filterButtonsByActivation pattern buttons = new_buttons
  where
    new_buttons = map fst filtered_buttons_idx
    filtered_buttons_idx = filter (\ (_, idx) -> testBit pattern idx) (zip buttons [0..])

decreaseJoltagesByButtons :: Joltages -> Buttons -> Joltages
decreaseJoltagesByButtons joltages buttons = new_joltages
  where
    new_joltages = zipWith (\jolt idx -> jolt - getDelta idx) joltages [0..]

    getDelta :: Int -> Int
    getDelta idx = sum $ map (\button -> fromEnum $ testBit button idx) buttons

decreaseJoltagesByActivation :: Joltages -> Buttons -> ActivationPattern -> Joltages
decreaseJoltagesByActivation joltages buttons pattern = new_joltages
  where
    new_buttons = filterButtonsByActivation pattern buttons
    new_joltages = decreaseJoltagesByButtons joltages new_buttons

countActive :: ActivationPattern -> Int
countActive = popCount

parseLineComplete :: String -> (Parity, Buttons, Joltages)
parseLineComplete line = (target, buttons, joltages)
  where
    (raw_target_str, rest) = case words line of
      (w:ws) -> (w, ws)
      [] -> error "It should never happen: line is empty"
    target_str = filter (\c -> c /= '[' && c /= ']') raw_target_str
    target = foldr (\c acc -> 2*acc + if c == '#' then 1 else 0) 0 target_str
    (expl_buttons, joltages) = parseButtonsJoltages rest
    buttons = reverse $ map decodedButtonToBinary expl_buttons

applyPressCountToLine :: String -> Int
applyPressCountToLine line = pressesForJoltages buttons joltages
  where
    (_, buttons, joltages) = parseLineComplete line
