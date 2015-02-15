module Gameboy.Memory (
  Memory(..)
) where

import Data.Word
import qualified Data.Vector as V

data Memory = Memory {
    mapBios :: Bool,
    biosRom :: V.Vector Word8,
    cartRom :: V.Vector Word8,
    graphicsRam :: V.Vector Word8,
    cartExternalRam :: V.Vector Word8,
    workingRam :: V.Vector Word8,
    spriteRam :: V.Vector Word8,
    zeroPageRam :: V.Vector Word8
  }
