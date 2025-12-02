module Utils where

stringToInt :: String -> Int
stringToInt = read

intToString :: Int -> String
intToString = show

splitOnce :: String -> Char -> Maybe (String, String)
splitOnce s c
  | null right = Nothing
  | otherwise = Just (left, tail right)
  where
    (left, right) = break (== c) s

