module Gameboy.Screen (
  Pixel(..),
  Screen,
  horizontalPixels,
  verticalPixels,
  pixelAt,
  testPattern
) where

import qualified Data.Vector as V

data Pixel = Black | DarkGray | LightGray | White

type Screen = V.Vector Pixel

horizontalPixels :: Int
horizontalPixels = 160

verticalPixels :: Int
verticalPixels = 144

pixelAt :: V.Vector Pixel -> Int -> Int -> Pixel
pixelAt screen x y = screen V.! (y * horizontalPixels + x)

testPattern :: Screen
testPattern = V.fromList $ take (horizontalPixels * verticalPixels) $ cycle [Black, Black, DarkGray, DarkGray, LightGray, LightGray, White, White, White]
