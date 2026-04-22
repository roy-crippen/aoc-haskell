{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

module Day02 where

import Control.DeepSeq (NFData)
import Data.ByteString.Char8 qualified as BS8
import Data.Char (isDigit)
import Data.FileEmbed (embedFile)
import Data.Vector.Unboxed qualified as VU
import GHC.Generics (Generic)
import Util (Solution (..), parseBs8IntUnsafe)

-- | embedded puzzle input
#if EXAMPLE
inputBs :: BS8.ByteString
inputBs = $(embedFile "data/day02/example.txt")

expectedP1, expectedP2 :: Int
expectedP1 = 42
expectedP2 = 42

#else
inputBs :: BS8.ByteString
inputBs = $(embedFile "data/day02/input.txt")

expectedP1, expectedP2 :: Int
expectedP1 = 390
expectedP2 = 439

#endif

-- | parsed context for both parts
newtype Ctx = MkCtx {xss :: [VU.Vector Int]}
  deriving (Generic, Show)

instance NFData Ctx

-- | solution definition for the runner
solutionDay02 :: Solution Ctx
solutionDay02 =
  MkSolution
    { day = 02,
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
parse s = MkCtx {xss = map parseLine (BS8.lines s)}
{-# INLINE parse #-}

parseLine :: BS8.ByteString -> VU.Vector Int
parseLine bs
  | BS8.null bs = VU.empty
  | otherwise = VU.fromList (go bs)
  where
    go :: BS8.ByteString -> [Int]
    go s
      | BS8.null s = []
      | otherwise =
          if BS8.null numBs
            then go rest
            else parseBs8IntUnsafe numBs : go rest
      where
        (numBs, rest) = BS8.span isDigit (BS8.dropWhile (== ' ') s)
{-# INLINE parseLine #-}

-- ------
-- common
-- ------

diffs :: VU.Vector Int -> VU.Vector Int
diffs xs = VU.zipWith (-) (VU.tail xs) xs
{-# INLINE diffs #-}

isValidDiffs :: VU.Vector Int -> Bool
isValidDiffs ds
  | VU.null ds = True
  | otherwise = VU.all (\d -> d * dir > 0 && abs d < 4) ds
  where
    d0 = VU.head ds
    dir = if d0 > 0 then 1 else -1
{-# INLINE isValidDiffs #-}

-- ------
-- part 1
-- ------

part1 :: Ctx -> Int
part1 ctx = sum [1 | xs <- ctx.xss, isValidDiffs (diffs xs)]

-- ------
-- part 2
-- ------

part2 :: Ctx -> Int
part2 ctx = sum [1 | xs <- ctx.xss, isValidPart2 xs]

isValidPart2 :: VU.Vector Int -> Bool
isValidPart2 xs
  | VU.length xs < 2 = True
  | isValidDiffs (diffs xs) = True
  | otherwise = any (isValidDiffs . diffs) (removals xs)
{-# INLINE isValidPart2 #-}

removals :: VU.Vector Int -> [VU.Vector Int]
removals v =
  [VU.slice 0 i v VU.++ VU.slice (i + 1) (n - i - 1) v | i <- [0 .. n - 1]]
  where
    n = VU.length v
{-# INLINE removals #-}