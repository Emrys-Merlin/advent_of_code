module Main where

import Control.Exception
import Control.DeepSeq
import Formatting
import Formatting.Clock
import System.Clock
import Options.Applicative
import System.FilePath ((</>))
import Text.Printf (printf)
import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08
import qualified Day09
import qualified Day10

-- Map to task solver
solve :: Int -> Int -> (String -> String)
solve day task = case (day, task) of
  (1, 1) -> Day01.task01
  (1, 2) -> Day01.task02
  (2, 1) -> Day02.task01
  (2, 2) -> Day02.task02
  (3, 1) -> Day03.task01
  (3, 2) -> Day03.task02
  (4, 1) -> Day04.task01
  (4, 2) -> Day04.task02
  (5, 1) -> Day05.task01
  (5, 2) -> Day05.task02
  (6, 1) -> Day06.task01
  (6, 2) -> Day06.task02
  (7, 1) -> Day07.task01
  (7, 2) -> Day07.task02
  (8, 1) -> Day08.task01
  (8, 2) -> Day08.task02
  (9, 1) -> Day09.task01
  (9, 2) -> Day09.task02
  (10, 1) -> Day10.task01
  (10, 2) -> Day10.task02
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
inputName :: Int -> String
inputName = printf "day%02d.txt"

exampleName :: Int -> Int -> String
exampleName = printf "day%02d_%02d.txt"


main :: IO ()
main = do
  opts <- execParser optsInfo
  let d = day opts
      t = task opts
      e = example opts
      solver = solve d t
      path = if e < 0
             then "../inputs" </> inputName d
             else "../examples" </> exampleName d e
  content <- readFile path
  start <- getTime Monotonic
  result <- evaluate $ force solver content
  end <- getTime Monotonic
  putStrLn result
  fprintLn timeSpecs start end
