{-# LANGUAGE ExistentialQuantification #-}

module Util where

import Control.Parallel.Strategies (NFData, parMap, rdeepseq)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.List (foldl')
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import System.IO.Unsafe (unsafePerformIO)

data Solution a = MkSolution
  { day :: Int,
    inputBytes :: BS8.ByteString,
    parseInput :: BS8.ByteString -> a,
    solvePart1 :: a -> Int,
    solvePart2 :: a -> Int,
    expectedPart1 :: Int,
    expectedPart2 :: Int
  }

-- Existential wrapper so we can put different Ctx types in one list
data SomeSolution where
  MkSomeSolution :: (NFData ctx) => Solution ctx -> SomeSolution

-- | Strictly reads a file in pure code.
--   Throws a runtime exception with a clear message if the file is missing or unreadable.
readFileUnsafe :: FilePath -> BS.ByteString
readFileUnsafe path = unsafePerformIO $ do BS.readFile path
{-# NOINLINE readFileUnsafe #-}

parseBs8IntUnsafe :: BS8.ByteString -> Int
parseBs8IntUnsafe s = case BS8.readInt s of
  Just (n, _) -> n
  Nothing -> error $ "Cannot parse as Int: " ++ show s

parseNumUnsafe :: (Integral a) => T.Text -> a
parseNumUnsafe s = case TR.signed TR.decimal $ T.strip s of
  Right (x, _) -> x
  _ -> error "Invalid number string in input"

-- Parallel foldl' with a combining function for partial results
parFoldl' :: (NFData b) => (b -> a -> b) -> (b -> b -> b) -> b -> [a] -> b
parFoldl' f g z xs = foldl' g z $ parMap rdeepseq (foldl' f z) chunks
  where
    -- Split list into chunks based on number of cores
    numCores = 24 -- Adjust based on system or use GHC.Conc.numCapabilities
    chunkSize = max 1 (length xs `div` numCores)
    chunks = chunkList chunkSize xs

-- Helper to split list into chunks
chunkList :: Int -> [a] -> [[a]]
chunkList _ [] = []
chunkList n xs
  | length xs <= n = [xs]
  | otherwise = take n xs : chunkList n (drop n xs)

kCombosWithoutReps :: Int -> [a] -> [[a]]
kCombosWithoutReps 0 _ = [[]]
kCombosWithoutReps _ [] = []
kCombosWithoutReps k (x : xs)
  | k < 0 || k > length (x : xs) = []
  | otherwise = [x : ys | ys <- kCombosWithoutReps (k - 1) xs] ++ kCombosWithoutReps k xs
