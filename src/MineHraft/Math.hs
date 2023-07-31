module MineHraft.Math where

import Data.Bits
import MineHraft.Packet.Types (VarInt)

uShiftR :: (Integral i) => i -> Int -> i
uShiftR n k = fromIntegral (fromIntegral n `shiftR` k :: Word)

unsafeToWord :: VarInt -> Word8
unsafeToWord = fromIntegral