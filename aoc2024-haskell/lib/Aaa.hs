module Aaa where

import Data.ByteString.Char8 qualified as BS8
import Data.Massiv.Array as A
import Data.Word (Word8)
import Day04 (inputBs)

cleanBs :: BS8.ByteString
cleanBs = BS8.filter (/= '\n') inputBs

flat :: Vector U Word8
flat = A.fromByteString Seq cleanBs

aInner :: Array U Ix2 Word8
aInner = A.resize' (Sz (10 :. 10)) flat

a :: Array U Ix2 Word8
a = computeAs U $ A.applyStencil (Padding (Sz2 3 3) (Sz2 3 3) (Fill 36)) idStencil aInner
