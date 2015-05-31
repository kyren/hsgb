module Gameboy.Util where

import Data.Bits
import Data.Word

makeWord :: Word8 -> Word8 -> Word16
makeWord l h = shift (fromIntegral h) 8 .|. fromIntegral l

lowByte :: Word16 -> Word8
lowByte = fromIntegral

highByte :: Word16 -> Word8
highByte w = fromIntegral (shift w (-8))
