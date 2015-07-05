module Gameboy.Util where

import Data.Bits
import Data.Word

makeWord16 :: Word8 -> Word8 -> Word16
makeWord16 l h = shift (fromIntegral h) 8 .|. fromIntegral l

lowByte :: Word16 -> Word8
lowByte = fromIntegral

highByte :: Word16 -> Word8
highByte w = fromIntegral (shift w (-8))
