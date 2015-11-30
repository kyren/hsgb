module Gameboy.Screen (
  Pixel(..),
  horizontalScreenPixels,
  verticalScreenPixels,
  Screen,
  testPattern,
  pixelAt,
  pixelGray,
  renderScreen,
  getScreen,
  saveScreenShot
) where

import Data.Bits
import Data.Word
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Codec.Picture as Picture
import Gameboy.Emulation

data Pixel = Black | DarkGray | LightGray | White

horizontalScreenPixels :: Int
horizontalScreenPixels = 160

verticalScreenPixels :: Int
verticalScreenPixels = 144

type Screen = V.Vector Pixel

testPattern :: Screen
testPattern = V.fromList $ take (horizontalScreenPixels * verticalScreenPixels) $ cycle [Black, Black, DarkGray, DarkGray, LightGray, LightGray, White, White, White]

pixelAt :: V.Vector Pixel -> Int -> Int -> Pixel
pixelAt screen x y = screen V.! (y * horizontalScreenPixels + x)

pixelGray :: Pixel -> Word8
pixelGray Black = 0
pixelGray DarkGray = 96
pixelGray LightGray = 192
pixelGray White = 255

renderScreen :: Screen -> Picture.Image Picture.Pixel8
renderScreen screen = Picture.generateImage go horizontalScreenPixels verticalScreenPixels
  where
    go x y = pixelGray (pixelAt screen x y)

saveScreenShot :: String -> Screen -> IO ()
saveScreenShot name screen = Picture.savePngImage name (Picture.ImageY8 (renderScreen screen))

getScreen :: EmulatorState -> Screen
getScreen state = V.generate (horizontalScreenPixels * verticalScreenPixels) gen
  where
    gen i = genPixel (i `rem` horizontalScreenPixels) (i `quot` horizontalScreenPixels)
    genPixel x y = colorAtTile (tileOf (x `quot` 8) (y `quot` 8)) (x `rem` 8, y `rem` 8)
    tileOf x y = fromIntegral (bgMapData state VU.! (y * 32 + x))
    colorAtTile tile (xTile, yTile) =
      case getBitPair (tile * 16 + yTile * 2) xTile of
        (False, False) -> White
        (False, True) -> LightGray
        (True, False) -> DarkGray
        (True, True) -> Black
    getBitPair bytePair xTile = (testBit byte1 column, testBit byte2 column)
      where
        byte1 = characterRam state VU.! bytePair
        byte2 = characterRam state VU.! (bytePair + 1)
        column = 8 - xTile
