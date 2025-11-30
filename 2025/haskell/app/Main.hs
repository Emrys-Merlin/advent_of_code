module Main where

import Options.Applicative
import System.FilePath ((</>))
import Text.Printf (printf)
import qualified Day01

-- Map to task solver
solve :: Int -> Int -> (String -> String)
solve day task = case (day, task) of
  (1, 1) -> Day01.task01
  (1, 2) -> Day01.task02
  _      -> \_ -> error "Unknown day/task"

-- CLI handling
data Options = Options
  { day :: Int
  , task :: Int
  , example :: Int
  } deriving (Show)

optionsParser :: Parser Options
optionsParser =
  Options
    <$> argument auto
        ( metavar "DAY"
       <> help "Day number (integer)")
    <*> argument auto
        ( metavar "TASK"
       <> help "Task number (integer)")
    <*> option auto
        ( long "example"
       <> short 'e'
       <> metavar "N"
       <> help "Example number (integer, -1 means disabled)"
       <> value (-1)
       <> showDefault )

optsInfo :: ParserInfo Options
optsInfo = info (optionsParser <**> helper)
  ( fullDesc
  <> progDesc "Run an AoC solution for a given day and task")

-- Build input file names
input_name :: Int -> String
input_name day = printf "day%02d.txt" day

example_name :: Int -> Int -> String
example_name day example = printf "day%02d_%02d.txt" day example


main :: IO ()
main = do
  opts <- execParser optsInfo
  let d = day opts
      t = task opts
      e = example opts
      solver = solve d t
      path = if e < 0
             then "../inputs" </> (input_name d)
             else "../examples" </> (example_name d e)
  content <- readFile path
  putStrLn $ solver content
