import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (foldM)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Day01 (solutionDay01)
import Day02 (solutionDay02)
import Day99 (solutionDay99)
-- ...

import Text.Printf (printf)
import Util (Solution (..), SomeSolution (..))

solutions :: [SomeSolution]
solutions =
  [ MkSomeSolution solutionDay01,
    MkSomeSolution solutionDay02,
    MkSomeSolution solutionDay99
    -- ...
  ]

main :: IO ()
main = do
  putStrLn ""
  putStrLn "     --------------- execution time ---------------"
  putStrLn "day |     parse      part 1      part 2       total"

  totalTime <- foldM runSolution 0 solutions

  putStrLn $ "\nTotal time: " ++ formatTotal totalTime
  where
    runSolution :: Double -> SomeSolution -> IO Double
    runSolution acc (MkSomeSolution MkSolution {..}) = do
      tStartParse <- getCurrentTime
      ctx <- evaluate $ force $ parseInput inputBytes
      tEndParse <- getCurrentTime
      let tParse = realToFrac (diffUTCTime tEndParse tStartParse) * 1_000_000

      tStart1 <- getCurrentTime
      v1 <- evaluate $ force $ solvePart1 ctx
      tEnd1 <- getCurrentTime
      let t1 = realToFrac (diffUTCTime tEnd1 tStart1) * 1_000_000

      tStart2 <- getCurrentTime
      v2 <- evaluate $ force $ solvePart2 ctx
      tEnd2 <- getCurrentTime
      let t2 = realToFrac (diffUTCTime tEnd2 tStart2) * 1_000_000

      let tTotal = tParse + t1 + t2

      putStrLn $ formatRow day tParse t1 t2 tTotal v1 v2 expectedPart1 expectedPart2

      return (acc + tTotal)

padTime :: Double -> String
padTime t =
  if t < 1000
    then printf "%8d" (round t :: Int) ++ "us"
    else printf "%8.2f" (t / 1000) ++ "ms"

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
    dayStr = if day < 10 then "0" ++ show day else show day

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
formatTotal t =
  if t < 1000
    then printf "%.0f" t ++ "us"
    else printf "%.2f" (t / 1000) ++ "ms"

-- todo
-- format in 2 decimal ms
-- add args day and best
-- green in under 100ms otherwise yellow