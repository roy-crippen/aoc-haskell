{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString.Char8 as BS8
import Data.Word (Word8)
import Grid
import Test.Tasty
import Test.Tasty.HUnit

bs :: BS8.ByteString
bs =
  "MMMSXXMASM\n\
  \MSAMXMSMSA\n\
  \AMXSXMAAMM\n\
  \MSAMASMSMX\n\
  \XMASAMXAMM\n\
  \XXAMMXXAMA\n\
  \SMSMSASXSS\n\
  \SAXAMASAAA\n\
  \MAMMMXMMMM\n\
  \MXMXAXMASX"

toRows :: BS8.ByteString -> [[Word8]]
toRows input =
  map (map (fromIntegral . fromEnum) . BS8.unpack) (BS8.lines input)

charToWord8 :: Char -> Word8
charToWord8 c = fromIntegral (fromEnum c)

tests :: TestTree
tests =
  testGroup
    "Grid module"
    [ testGroup
        "Creation"
        [ testCase "createGrid pad=0 builds correct 10×10 grid" $ do
            let grid = fromByteString 10 10 bs
            rows grid @?= 10
            cols grid @?= 10
            padCnt grid @?= 0
        ],
      testGroup
        "Lookups"
        [ testCase "lookupGrid returns correct values" $ do
            let grid = fromByteString 10 10 bs
            lookupGrid grid (0, 0) @?= charToWord8 'M'
            lookupGrid grid (4, 4) @?= charToWord8 'A'
            lookupGrid grid (9, 9) @?= charToWord8 'X'
        ],
      testGroup
        "isInBounds"
        [ testCase "isInBounds works correctly" $ do
            let grid = fromByteString 10 10 bs
            isInBounds grid (0, 0) @?= True
            isInBounds grid (2, 2) @?= True
            isInBounds grid (-1, 5) @?= False
            isInBounds grid (16, 5) @?= False
        ],
      testGroup
        "Find functions"
        [ testCase "findPositions returns all matching positions" $ do
            let grid = fromByteString 10 10 bs
            let xs = findPositions (== charToWord8 'X') grid
            length xs @?= 19 -- known number of X's in this input
            (0, 4) `elem` xs @?= True, -- first X is at (0,4)
          testCase "findFirstPosition returns first occurrence in row-major order" $ do
            let grid = fromByteString 10 10 bs
            findFirstPosition (== charToWord8 'M') grid @?= Just (0, 0)
            findFirstPosition (== charToWord8 'X') grid @?= Just (0, 4) -- corrected
            findFirstPosition (== charToWord8 '$') grid @?= Nothing
        ],
      testGroup
        "Neighbors"
        [ testCase "neighbors4 and unsafeNeighbors4" $ do
            let grid = fromByteStringPadded 10 10 1 '$' bs
            length (neighbors4 grid 5 5) @?= 4
            length (unsafeNeighbors4 grid 5 5) @?= 4
        ],
      testGroup
        "Show instance"
        [ testCase "show displays inner grid" $ do
            let grid = fromByteString 10 10 bs
            let s = show grid
            length (lines s) @?= 11
        ]
    ]

main :: IO ()
main = defaultMain tests