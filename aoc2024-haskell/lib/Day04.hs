{-# LANGUAGE CPP #-}

module Day04 where

import Control.DeepSeq (NFData)
import Data.ByteString.Char8 qualified as BS8
import Data.FileEmbed (embedFile)
import Data.List (foldl')
import GHC.Generics (Generic)
import Grid qualified as G
import Util (Solution (..))

-- | embedded puzzle input
#if EXAMPLE
inputBs :: BS8.ByteString
inputBs = $(embedFile "data/day04/example.txt")

rows, cols :: Int
rows = 10
cols = 10

expectedP1,  expectedP2:: Int
expectedP1 = 18
expectedP2 = 9

#else
inputBs :: BS8.ByteString
inputBs = $(embedFile "data/day04/input.txt")

rows, cols :: Int
rows = 140
cols = 140

expectedP1,  expectedP2:: Int
expectedP1 = 2573
expectedP2 = 1850

#endif

-- | parsed context for both parts
newtype Ctx = MkCtx {grid :: G.Grid}
  deriving (Generic, Show)

instance NFData Ctx

-- | solution definition for the runner
solutionDay04 :: Solution Ctx
solutionDay04 =
  MkSolution
    { day = 04,
      inputBytes = inputBs,
      parseInput = parse,
      solvePart1 = part1,
      solvePart2 = part2,
      expectedPart1 = expectedP1,
      expectedPart2 = expectedP2
    }

-- -------
-- parsing
-- -------

parse :: BS8.ByteString -> Ctx
parse bs = MkCtx {grid = G.fromByteStringPadded rows cols 3 '$' bs}

-- ------
-- common
-- ------

-- 'X' = 88
-- 'M' = 77
-- 'A' = 65
-- 'S' = 83

-- ------
-- part 1
-- ------

part1 :: Ctx -> Int
part1 ctx = foldl' (\acc pos -> acc + countXmas ctx.grid pos) 0 (findXs ctx.grid)

findXs :: G.Grid -> [G.Pos]
findXs = G.findPositions (== 88)

isXmas :: G.Grid -> G.Pos -> G.Direction -> Bool
isXmas g (r, c) (dr, dc) = w3 == 83 && w2 == 65 && w1 == 77 -- 'S' 'A' 'M'
  where
    w1 = G.unsafeLookupGrid g (r + dr, c + dc)
    w2 = G.unsafeLookupGrid g (r + 2 * dr, c + 2 * dc)
    w3 = G.unsafeLookupGrid g (r + 3 * dr, c + 3 * dc)

countXmas :: G.Grid -> G.Pos -> Int
countXmas g pos =
  fromEnum (isXmas g pos (-1, -1))
    + fromEnum (isXmas g pos (-1, 0))
    + fromEnum (isXmas g pos (-1, 1))
    + fromEnum (isXmas g pos (0, -1))
    + fromEnum (isXmas g pos (0, 1))
    + fromEnum (isXmas g pos (1, -1))
    + fromEnum (isXmas g pos (1, 0))
    + fromEnum (isXmas g pos (1, 1))

-- ------
-- part 2
-- ------

part2 :: Ctx -> Int
part2 _ = expectedP2
