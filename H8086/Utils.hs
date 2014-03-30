module H8086.Utils where

import Data.Word
import Data.Bits

lo :: Word16 -> Word8
lo word = fromIntegral $ (.&.) word 0xff

hi :: Word16 -> Word8
hi word = fromIntegral $ (.&.) (shiftR word 8) 0xff

fromLoHi :: Word8 -> Word8 -> Word16
fromLoHi lo hi = (.|.) (fromIntegral lo) $ shift (fromIntegral hi) 8
