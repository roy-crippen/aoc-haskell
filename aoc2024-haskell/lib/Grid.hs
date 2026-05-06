{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Grid where

import Control.DeepSeq
import Data.ByteString.Char8 qualified as BS8
import Data.Char (chr)
import Data.Massiv.Array as A
import Data.Massiv.Array.Unsafe (unsafeIndex)
import Data.Word (Word8)

-- Core types
type Pos = (Int, Int)

data Grid = Grid
  { gridData :: Array U Ix2 Word8,
    rows :: Int,
    cols :: Int,
    padCnt :: Int
  }

instance NFData Grid where
  rnf :: Grid -> ()
  rnf Grid {..} = gridData `deep` rnf rows `seq` rnf cols `seq` rnf padCnt
    where
      deep arr x = rnf arr `seq` x

instance Show Grid where
  show :: Grid -> String
  show g@Grid {..} = unlines $ header : body
    where
      header = "  " ++ [lastDigit c | c <- [0 .. cols - 1]]
      lastDigit c = head (show (c `mod` 10))

      body = [showRow r | r <- [0 .. rows - 1]]

      showRow r =
        let rowLabel = show (r `mod` 10) ++ " "
            cells = [showCell (lookupGrid g (r, c)) | c <- [0 .. cols - 1]]
         in rowLabel ++ concat cells

      showCell w = [chr (fromIntegral w)]

-- ================================================================
-- Inner array (shared, cheap) — now takes explicit dimensions
-- ================================================================

innerArr :: Int -> Int -> BS8.ByteString -> Array U Ix2 Word8
innerArr innerR innerC bs
  | BS8.null bs = A.empty
  | otherwise =
      let cleanBs = BS8.filter (/= '\n') bs
          flat = A.fromByteString Seq cleanBs
       in A.resize' (Sz2 innerR innerC) flat

-- ================================================================
-- Grid creation using applyStencil for padding (efficient)
-- ================================================================

fromByteString :: Int -> Int -> BS8.ByteString -> Grid
fromByteString innerR innerC = fromByteStringPadded innerR innerC 0 ' '

fromByteStringPadded :: Int -> Int -> Int -> Char -> BS8.ByteString -> Grid
fromByteStringPadded innerR innerC pad padVal bs
  | pad < 0 = error "negative padding"
  | pad == 0 = Grid {gridData = innerArr innerR innerC bs, rows = innerR, cols = innerC, padCnt = 0}
  | otherwise = Grid {gridData = padded, rows = totalR, cols = totalC, padCnt = pad}
  where
    inner = innerArr innerR innerC bs
    padW8 = fromIntegral (fromEnum padVal)
    totalR = innerR + 2 * pad
    totalC = innerC + 2 * pad
    padding = Padding (Sz2 pad pad)

    padded :: Array U Ix2 Word8
    padded = computeAs U $ A.applyStencil (padding (Sz2 pad pad) (Fill padW8)) idStencil inner

-- ================================================================
-- Lookups / Updates / Rest
-- ================================================================

lookupGrid :: Grid -> Pos -> Word8
lookupGrid g (r, c) = gridData g ! (r :. c)

unsafeLookupGrid :: Grid -> Pos -> Word8
unsafeLookupGrid g (r, c) = unsafeIndex (gridData g) (r :. c)

updateGrid :: Grid -> Pos -> Word8 -> Grid
updateGrid g pos val =
  if isInBounds g pos
    then g {gridData = computeP $ A.imap updateOne (gridData g)}
    else g
  where
    updateOne ix old = if ix == pos' then val else old
      where
        pos' = case pos of (r, c) -> r :. c

unsafeUpdateGrid :: Grid -> Pos -> Word8 -> Grid
unsafeUpdateGrid g pos val =
  g {gridData = computeP $ A.imap updateOne (gridData g)}
  where
    pos' = case pos of (r, c) -> r :. c
    updateOne ix old = if ix == pos' then val else old

-- Movement, directions, bounds, find, neighbors (same as before)
moveN, moveS, moveE, moveW :: Pos -> Pos
moveN (r, c) = (r - 1, c)
moveS (r, c) = (r + 1, c)
moveE (r, c) = (r, c + 1)
moveW (r, c) = (r, c - 1)

moveNE, moveNW, moveSE, moveSW :: Pos -> Pos
moveNE (r, c) = (r - 1, c + 1)
moveNW (r, c) = (r - 1, c - 1)
moveSE (r, c) = (r + 1, c + 1)
moveSW (r, c) = (r + 1, c - 1)

type Direction = (Int, Int)

cardinalDirections :: [Direction]
cardinalDirections = [(-1, 0), (1, 0), (0, -1), (0, 1)]

diagonalDirections :: [Direction]
diagonalDirections = [(-1, -1), (-1, 1), (1, -1), (1, 1)]

allDirections :: [Direction]
allDirections = cardinalDirections ++ diagonalDirections

isInBounds :: Grid -> Pos -> Bool
isInBounds Grid {..} (r, c) =
  r >= 0 && r < rows && c >= 0 && c < cols

findPositions :: (Word8 -> Bool) -> Grid -> [Pos]
findPositions p g@Grid {..} =
  [(r, c) | r <- [0 .. rows - 1], c <- [0 .. cols - 1], p (lookupGrid g (r, c))]

findFirstPosition :: (Word8 -> Bool) -> Grid -> Maybe Pos
findFirstPosition p g@Grid {..} = go 0 0
  where
    go r c
      | r >= rows = Nothing
      | c >= cols = go (r + 1) 0
      | p (lookupGrid g (r, c)) = Just (r, c)
      | otherwise = go r (c + 1)

neighbors4 :: Grid -> Int -> Int -> [Word8]
neighbors4 g r c =
  [ lookupGrid g (nr, nc) | (di, dj) <- cardinalDirections, let nr = r + di; nc = c + dj, isInBounds g (nr, nc)
  ]

neighbors8 :: Grid -> Int -> Int -> [Word8]
neighbors8 g r c =
  [ lookupGrid g (nr, nc) | (di, dj) <- allDirections, let nr = r + di; nc = c + dj, isInBounds g (nr, nc)
  ]

unsafeNeighbors4 :: Grid -> Int -> Int -> [Word8]
unsafeNeighbors4 g r c = [unsafeLookupGrid g (r + di, c + dj) | (di, dj) <- cardinalDirections]

unsafeNeighbors8 :: Grid -> Int -> Int -> [Word8]
unsafeNeighbors8 g r c = [unsafeLookupGrid g (r + di, c + dj) | (di, dj) <- allDirections]

neighbors4Pos :: Grid -> Pos -> [Word8]
neighbors4Pos g = uncurry (neighbors4 g)

neighbors8Pos :: Grid -> Pos -> [Word8]
neighbors8Pos g = uncurry (neighbors8 g)