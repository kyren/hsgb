module Gameboy.Util where

import Data.Bits
import Data.Word

{-# INLINE makeWord8 #-}
makeWord8 :: Word8 -> Word8 -> Word8
makeWord8 ln hn = shift (fromIntegral hn) 4 .|. fromIntegral ln

{-# INLINE lowNibble #-}
lowNibble :: Word8 -> Word8
lowNibble w = w .&. 0x0f

{-# INLINE highNibble #-}
highNibble :: Word8 -> Word8
highNibble w = shift w (-4) .&. 0x0f

{-# INLINE makeWord16 #-}
makeWord16 :: Word8 -> Word8 -> Word16
makeWord16 l h = shift (fromIntegral h) 8 .|. fromIntegral l

{-# INLINE lowByte #-}
lowByte :: Word16 -> Word8
lowByte = fromIntegral

{-# INLINE highByte #-}
highByte :: Word16 -> Word8
highByte w = fromIntegral (shift w (-8))

-- add two 8 bit numbers, return the result, as well as the carry 3 bit flag
-- and the carry 7 bit flag.
{-# INLINE add8 #-}
add8 :: Word8 -> Word8 -> (Word8, Bool, Bool)
add8 a b = (a + b, carry3, carry7)
  where
    carry3 = (lowNibble a + lowNibble b) > 0x0f
    carry7 = (fromIntegral a + fromIntegral b) > (0xff :: Word16)

-- subtract two 8 bit numbers, return the result, as well as the borrow 4 bit flag
-- and the borrow 8 bit flag.
{-# INLINE sub8 #-}
sub8 :: Word8 -> Word8 -> (Word8, Bool, Bool)
sub8 a b = (a - b, borrow4, borrow8)
  where
    borrow4 = lowNibble b > lowNibble a
    borrow8 = b > a

-- add two 16 bit numbers, return the result, as well as the carry 11 bit flag
-- and the carry 15 bit flag.
{-# INLINE add16 #-}
add16 :: Word16 -> Word16 -> (Word16, Bool, Bool)
add16 a b = (a + b, carry11, carry15)
  where
    low12 n = makeWord16 (lowNibble (highByte n)) (lowByte n)
    carry11 = (low12 a + low12 b) > 0xfff
    carry15 = (fromIntegral a + fromIntegral b) > (0xffff :: Word32)

{-# INLINE rotLC #-}
rotLC :: Word8 -> (Word8, Bool)
rotLC b = (r, c)
  where
    c = testBit b 7
    r = rotate b 1

{-# INLINE rotL #-}
rotL :: Word8 -> Bool -> (Word8, Bool)
rotL b c = (br, cr)
  where
    cr = testBit b 7
    bs = shift b 1
    br = if c then setBit bs 0 else bs

{-# INLINE rotRC #-}
rotRC :: Word8 -> (Word8, Bool)
rotRC b = (r, c)
  where
    c = testBit b 0
    r = rotate b (-1)

{-# INLINE rotR #-}
rotR :: Word8 -> Bool -> (Word8, Bool)
rotR b c = (br, cr)
  where
    cr = testBit b 0
    bs = shift b (-1)
    br = if c then setBit bs 7 else bs
