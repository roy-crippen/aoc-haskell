{-# LANGUAGE CPP #-}

module Day03 where

import Control.DeepSeq (NFData)
import Data.ByteString.Char8 qualified as BS8
import Data.FileEmbed (embedFile)
import GHC.Generics (Generic)
import Util (Solution (..))

-- | embedded puzzle input
#if EXAMPLE
inputBs :: BS8.ByteString
inputBs = $(embedFile "data/day03/example.txt")

expectedP1,  expectedP2:: Int
expectedP1 = 42
expectedP2 = 42

#else
inputBs :: BS8.ByteString
inputBs = $(embedFile "data/day03/input.txt")

expectedP1,  expectedP2:: Int
expectedP1 = 169021493
expectedP2 = 111762583

#endif

-- | parsed context for both parts
data Ctx = MkCtx {fullBs, noDontsBs :: BS8.ByteString}
  deriving (Generic, Show)

instance NFData Ctx

-- | solution definition for the runner
solutionDay03 :: Solution Ctx
solutionDay03 =
  MkSolution
    { day = 03,
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
parse bs = MkCtx {fullBs = BS8.strip bs, noDontsBs = removeDonts bs}

removeDonts :: BS8.ByteString -> BS8.ByteString
removeDonts bs = keep'
  where
    (left, right) = BS8.breakSubstring "don't()" bs
    (_, keep) = BS8.breakSubstring "do()" right
    keep' = if BS8.null keep then left else BS8.append left (removeDonts (BS8.drop 4 keep))

-- ------
-- common
-- ------

parseMulPairs :: BS8.ByteString -> [Int]
parseMulPairs bs = go bs []
  where
    go :: BS8.ByteString -> [Int] -> [Int]
    go s acc
      | BS8.null s = acc
      | otherwise = acc'
      where
        (_, found) = BS8.breakSubstring "mul(" s
        (mv, remaining) = evalPair $ BS8.drop 4 found
        acc' = case mv of
          Just v -> go remaining (v : acc)
          Nothing -> go remaining acc

evalPair :: BS8.ByteString -> (Maybe Int, BS8.ByteString)
evalPair bs =
  case BS8.break (== ',') bs of
    (a, rest)
      | BS8.null rest -> (Nothing, bs)
      | otherwise ->
          case BS8.break (== ')') (BS8.drop 1 rest) of
            (b, after)
              | BS8.null after -> (Nothing, bs)
              | otherwise ->
                  case (BS8.readInt a, BS8.readInt b) of
                    (Just (x, ""), Just (y, "")) -> (Just (x * y), BS8.drop 1 after)
                    _ -> (Nothing, bs)

-- ------
-- part 1
-- ------

part1 :: Ctx -> Int
part1 ctx = sum $ parseMulPairs ctx.fullBs

-- ------
-- part 2
-- ------

part2 :: Ctx -> Int
part2 ctx = sum $ parseMulPairs ctx.noDontsBs
