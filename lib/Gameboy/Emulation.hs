module Gameboy.Emulation where

import Data.Word
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

data Pixel = Black | DarkGray | LightGray | White

horizontalScreenPixels :: Int
horizontalScreenPixels = 160

verticalScreenPixels :: Int
verticalScreenPixels = 144

type Screen = V.Vector Pixel

pixelAt :: V.Vector Pixel -> Int -> Int -> Pixel
pixelAt screen x y = screen V.! (y * horizontalScreenPixels + x)

testPattern :: Screen
testPattern = V.fromList $ take (horizontalScreenPixels * verticalScreenPixels) $ cycle [Black, Black, DarkGray, DarkGray, LightGray, LightGray, White, White, White]

data FrozenState = FrozenState {
    interruptsEnabled :: Bool,
    stackPointer :: Word16,
    programCounter :: Word16,

    aRegister, bRegister, cRegister, dRegister, eRegister, hRegister, lRegister, fRegister :: Word8,

    cartridgeRomBank0 :: VU.Vector Word8,
    internalRamBank0 :: VU.Vector Word8,
    zeroPage :: VU.Vector Word8,

    bgMapData :: VU.Vector Word8,
    characterRam :: VU.Vector Word8
  }

renderFrame :: FrozenState -> Screen
renderFrame = undefined

stepFrame :: FrozenState -> FrozenState
stepFrame = undefined
