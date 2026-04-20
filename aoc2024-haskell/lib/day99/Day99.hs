{-# LANGUAGE CPP #-}

module Day99 where

import Control.DeepSeq (NFData)
import Data.ByteString.Char8 qualified as BS8
import Data.FileEmbed (embedFile)
import GHC.Generics (Generic)
import Util (Solution (..))

-- | embedded puzzle input
#if EXAMPLE
inputBs :: BS8.ByteString
inputBs = $(embedFile "lib/day99/example.txt")

expectedP1,  expectedP2:: Int
expectedP1 = 42
expectedP2 = 42

#else
inputBs :: BS8.ByteString
inputBs = $(embedFile "lib/day99/input.txt")

expectedP1,  expectedP2:: Int
expectedP1 = 42
expectedP2 = 42

#endif

-- | parsed context for both parts
data Ctx = MkCtx {}
  deriving (Generic, Show)

instance NFData Ctx

-- | solution definition for the runner
solutionDay99 :: Solution Ctx
solutionDay99 =
  MkSolution
    { day = 99,
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
parse _ = MkCtx {}

-- ------
-- common
-- ------

-- ------
-- part 1
-- ------

part1 :: Ctx -> Int
part1 _ = expectedP1

-- ------
-- part 2
-- ------

part2 :: Ctx -> Int
part2 _ = expectedP2
