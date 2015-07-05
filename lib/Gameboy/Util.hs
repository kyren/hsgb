module Gameboy.Util where

import Data.Bits
import Data.Word

makeWord16 :: Word8 -> Word8 -> Word16
makeWord16 l h = shift (fromIntegral h) 8 .|. fromIntegral l

lowNibble :: Word8 -> Word8
lowNibble w = (w .&. 0x0f)

highNibble :: Word8 -> Word8
highNibble w = shift w (-4) .&. 0x0f

lowByte :: Word16 -> Word8
lowByte = fromIntegral

highByte :: Word16 -> Word8
highByte w = fromIntegral (shift w (-8))

-- add two 8 bit numbers, return the result, as well as the carry 3 bit flag
-- and the carry 7 bit flag.
add8 :: Word8 -> Word8 -> (Word8, Bool, Bool)
add8 a b = (a + b, carry3, carry7)
  where
    carry3 = (lowNibble a + lowNibble b) > 0x0f
    carry7 = (fromIntegral a + fromIntegral b) > (0xff :: Word16)

-- subtract two 8 bit numbers, return the result, as well as the borrow 4 bit flag
-- and the borrow 8 bit flag.
sub8 :: Word8 -> Word8 -> (Word8, Bool, Bool)
sub8 a b = (a - b, borrow4, borrow8)
  where
    borrow4 = lowNibble b > lowNibble a
    borrow8 = b > a
