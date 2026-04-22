{-# LANGUAGE CPP #-}

module Day01 where

import Control.DeepSeq (NFData)
import Data.ByteString.Char8 qualified as BS8
import Data.FileEmbed (embedFile)
import Data.IntMap.Strict qualified as IntMap
import Data.Vector.Algorithms.Intro qualified as VA
import Data.Vector.Unboxed qualified as VU
import GHC.Generics (Generic)
import Util (Solution (..), parseBs8IntUnsafe)

-- | embedded puzzle input
#if EXAMPLE
inputBs :: BS8.ByteString
inputBs = $(embedFile "data/day01/example.txt")

expectedP1,  expectedP2:: Int
expectedP1 = 11
expectedP2 = 31

#else
inputBs :: BS8.ByteString
inputBs = $(embedFile "data/day01/input.txt")

expectedP1,  expectedP2:: Int
expectedP1 = 2086478
expectedP2 = 24941624

#endif

-- | parsed context for both parts
data Ctx = MkCtx {xs, ys :: VU.Vector Int}
  deriving (Generic, Show)

instance NFData Ctx

-- | solution definition for the runner
solutionDay01 :: Solution Ctx
solutionDay01 =
  MkSolution
    { day = 01,
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
parse bs =
  MkCtx
    { xs = VU.fromListN n lefts,
      ys = VU.fromListN n rights
    }
  where
    !ls = BS8.lines (BS8.strip bs)
    n = length ls
    (lefts, rights) = unzip (map parsePair ls)

parsePair :: BS8.ByteString -> (Int, Int)
parsePair line =
  case BS8.words line of
    [a, b] -> (parseBs8IntUnsafe a, parseBs8IntUnsafe b)
    _ -> error "Invalid pair on line"

-- ------
-- part 1
-- ------

part1 :: Ctx -> Int
part1 = solve . sortVectors

sortVectors :: Ctx -> Ctx
sortVectors ctx =
  MkCtx
    { xs = VU.modify VA.sort ctx.xs,
      ys = VU.modify VA.sort ctx.ys
    }

solve :: Ctx -> Int
solve ctx =
  VU.sum $ VU.zipWith (\x y -> abs (x - y)) ctx.xs ctx.ys

-- ------
-- part 2
-- ------

part2 :: Ctx -> Int
part2 ctx = VU.foldl' f 0 ctx.xs
  where
    f acc x = acc + x * IntMap.findWithDefault 0 x (buildFreq ctx.ys)

buildFreq :: VU.Vector Int -> IntMap.IntMap Int
buildFreq = VU.foldl' (\m v -> IntMap.insertWith (+) v 1 m) IntMap.empty
