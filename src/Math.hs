module Math where

import Data.Bits
import Data.Word

uShiftR :: (Integral i) => i -> Int -> i
uShiftR n k = fromIntegral (fromIntegral n `shiftR` k :: Word)

unsafeToWord :: Integer -> Word8
unsafeToWord i = fromInteger i :: Word8
