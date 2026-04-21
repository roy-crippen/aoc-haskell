import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (foldM, replicateM)
import Data.List (minimumBy)
import Data.Ord (comparing)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Day01 (solutionDay01)
import Day02 (solutionDay02)
import Day99 (solutionDay99)
import Options.Applicative
-- ... import other days as needed

import Text.Printf (printf)
import Util (Solution (..), SomeSolution (..))

-- Colors matching your Roc definitions
blue, green, yellow, reset :: String
blue = "\ESC[0;34m"
green = "\ESC[38;5;46m"
yellow = "\ESC[38;5;226m"
reset = "\ESC[0m"

solutions :: [SomeSolution]
solutions =
  [ MkSomeSolution solutionDay01,
    MkSomeSolution solutionDay02,
    MkSomeSolution solutionDay99
    -- ...
  ]

data Options = Options
  { optDay :: Maybe Int,
    optBest :: Bool
  }

optionsParser :: Parser Options
optionsParser =
  Options
    <$> optional
      ( option
          auto
          ( long "day"
              <> short 'd'
              <> metavar "DAY"
              <> help "Run only this specific day"
          )
      )
    <*> switch
      ( long "best"
          <> short 'b'
          <> help "Run each day 10 times and report the best total time"
      )

main :: IO ()
main = do
  Options {..} <-
    execParser $
      info
        (optionsParser <**> helper)
        ( fullDesc
            <> progDesc "Advent of Code 2024 solutions runner"
            <> header "AOC 2024 - Haskell"
        )

  putStrLn ""
  putStrLn $ blue ++ "     --------------- execution time ---------------" ++ reset
  putStrLn $ blue ++ "day |     parse      part 1      part 2       total" ++ reset

  totalTime <- case optDay of
    Nothing -> foldM (runSolution optBest) 0 solutions
    Just d -> case filter (matchesDay d) solutions of
      [] -> do
        putStrLn $ "\n" ++ yellow ++ "Day " ++ show d ++ " not found!" ++ reset
        return 0.0
      [sol] -> runSolution optBest 0.0 sol
      _ -> error "Duplicate day number in solutions list"

  putStrLn $
    "\n"
      ++ blue
      ++ "Total time: "
      ++ reset
      ++ green
      ++ formatTotal totalTime
      ++ reset
  where
    matchesDay :: Int -> SomeSolution -> Bool
    matchesDay target (MkSomeSolution MkSolution {day}) = day == target

    runSolution :: Bool -> Double -> SomeSolution -> IO Double
    runSolution useBest acc sol = do
      runs <-
        if useBest
          then replicateM 10 (timeOneRun sol)
          else (: []) <$> timeOneRun sol

      let bestTotal = minimumBy (comparing fifth) runs
          bestParse = third bestTotal
          bestP1 = fourth bestTotal
          bestP2 = fifth bestTotal -- wait, indexing fixed below
      let v1 = solvePart1Result sol
          v2 = solvePart2Result sol

      putStrLn $
        formatRow
          (getDay sol)
          bestParse
          bestP1
          bestP2
          (fifth bestTotal)
          v1
          v2
          (getExpected1 sol)
          (getExpected2 sol)

      return (acc + fifth bestTotal)

    -- Time one full run: returns (total, parse, p1, p2)
    timeOneRun :: SomeSolution -> IO (Double, Double, Double, Double)
    timeOneRun (MkSomeSolution MkSolution {parseInput, solvePart1, solvePart2, inputBytes}) = do
      t0 <- getCurrentTime
      ctx <- evaluate $ force $ parseInput inputBytes
      t1 <- getCurrentTime
      let tParse = realToFrac (diffUTCTime t1 t0) * 1_000_000

      t2 <- getCurrentTime
      _ <- evaluate $ force $ solvePart1 ctx
      t3 <- getCurrentTime
      let t1Time = realToFrac (diffUTCTime t3 t2) * 1_000_000

      t4 <- getCurrentTime
      _ <- evaluate $ force $ solvePart2 ctx
      t5 <- getCurrentTime
      let t2Time = realToFrac (diffUTCTime t5 t4) * 1_000_000

      let tTotal = tParse + t1Time + t2Time
      return (tTotal, tParse, t1Time, t2Time)

    -- Field extractors (no shadowing)
    getDay :: SomeSolution -> Int
    getDay (MkSomeSolution MkSolution {day}) = day

    getExpected1, getExpected2 :: SomeSolution -> Int
    getExpected1 (MkSomeSolution MkSolution {expectedPart1}) = expectedPart1
    getExpected2 (MkSomeSolution MkSolution {expectedPart2}) = expectedPart2

    solvePart1Result, solvePart2Result :: SomeSolution -> Int
    solvePart1Result (MkSomeSolution MkSolution {parseInput, solvePart1, inputBytes}) =
      solvePart1 (parseInput inputBytes)
    solvePart2Result (MkSomeSolution MkSolution {parseInput, solvePart2, inputBytes}) =
      solvePart2 (parseInput inputBytes)

    -- Tuple helpers
    fifth (_, _, _, x) = x
    third (_, x, _, _) = x
    fourth (_, _, x, _) = x

padTime :: Double -> String
padTime micros =
  let ms = micros / 1000.0
      col = if ms < 100.0 then green else yellow
   in col ++ printf "%8.2f" ms ++ "ms" ++ reset

formatRow :: Int -> Double -> Double -> Double -> Double -> Int -> Int -> Int -> Int -> String
formatRow day tParse t1 t2 tTotal v1 v2 exp1 exp2 =
  dayStr
    ++ "  |"
    ++ padTime tParse
    ++ "  "
    ++ padTime t1
    ++ "  "
    ++ padTime t2
    ++ "  "
    ++ padTime tTotal
    ++ errorInfo
  where
    dayStr = blue ++ (if day < 10 then "0" ++ show day else show day) ++ reset

    errorInfo
      | v1 == exp1 && v2 == exp2 = ""
      | otherwise = "\n" ++ formatError day v1 v2 exp1 exp2

formatError :: Int -> Int -> Int -> Int -> Int -> String
formatError day v1 v2 exp1 exp2 =
  unlines
    [ "  → Day " ++ show day ++ " failed:",
      "      Part 1: expected " ++ show exp1 ++ " but got " ++ show v1,
      "      Part 2: expected " ++ show exp2 ++ " but got " ++ show v2
    ]

formatTotal :: Double -> String
formatTotal micros =
  let ms = micros / 1000.0
   in printf "%.2f" ms ++ "ms"