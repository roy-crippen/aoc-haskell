import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (foldM, replicateM)
import Data.List (minimumBy)
import Data.Ord (comparing)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Day01 (solutionDay01)
import Day02 (solutionDay02)
import Day03 (solutionDay03)
import Day99 (solutionDay99)
import Options.Applicative
import Text.Printf (printf)
import Util (Solution (..), SomeSolution (..))

-- colors
blue, green, yellow, reset :: String
blue = "\ESC[0;34m"
green = "\ESC[38;5;46m"
yellow = "\ESC[38;5;226m"
reset = "\ESC[0m"

-- ... existing imports and color definitions ...

solutions :: [SomeSolution]
solutions =
  [ MkSomeSolution solutionDay01,
    MkSomeSolution solutionDay02,
    MkSomeSolution solutionDay03,
    MkSomeSolution solutionDay99
  ]

data Options = Options
  { optDay :: Maybe Int,
    optBest :: Bool
  }

data Results = MkResults
  { tParse, tP1, tP2, tTotal :: Double,
    vP1, vP2 :: Int
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

matchesDay :: Int -> SomeSolution -> Bool
matchesDay target (MkSomeSolution MkSolution {day}) = day == target

runSolution :: Bool -> Double -> SomeSolution -> IO Double
runSolution useBest acc someSolution@(MkSomeSolution sol) = do
  runs <-
    if useBest
      then replicateM 10 (timeOneRun someSolution)
      else (: []) <$> timeOneRun someSolution

  let best = minimumBy (comparing (.tTotal)) runs

  putStrLn $
    formatRow
      sol.day
      best.tParse
      best.tP1
      best.tP2
      best.tTotal
      best.vP1
      best.vP2
      sol.expectedPart1
      sol.expectedPart2

  return (acc + best.tTotal)

timeOneRun :: SomeSolution -> IO Results
timeOneRun (MkSomeSolution MkSolution {..}) = do
  t0 <- getCurrentTime
  ctx <- evaluate $ force $ parseInput inputBytes
  t1 <- getCurrentTime
  let tParse = realToFrac (diffUTCTime t1 t0) * 1_000_000

  t2 <- getCurrentTime
  v1 <- evaluate $ force $ solvePart1 ctx
  t3 <- getCurrentTime
  let tP1 = realToFrac (diffUTCTime t3 t2) * 1_000_000

  t4 <- getCurrentTime
  v2 <- evaluate $ force $ solvePart2 ctx
  t5 <- getCurrentTime
  let tP2 = realToFrac (diffUTCTime t5 t4) * 1_000_000

  let tTotal = tParse + tP1 + tP2

  return
    MkResults
      { tParse = tParse,
        tP1 = tP1,
        tP2 = tP2,
        tTotal = tTotal,
        vP1 = v1,
        vP2 = v2
      }

padTime :: Double -> String
padTime micros =
  let ms = micros / 1000.0
      col = if ms < 100.0 then green else yellow
   in col ++ printf "%8.2f" ms ++ "ms" ++ reset

formatRow :: Int -> Double -> Double -> Double -> Double -> Int -> Int -> Int -> Int -> String
formatRow day tParse tP1 tP2 tTotal v1 v2 exp1 exp2 =
  dayStr
    ++ "  |"
    ++ padTime tParse
    ++ "  "
    ++ padTime tP1
    ++ "  "
    ++ padTime tP2
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