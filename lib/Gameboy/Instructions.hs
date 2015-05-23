module Gameboy.Instructions where

import Data.Word
import Gameboy.CPU

data Load8Target = RegisterTarget Register | AtHLTarget deriving Show

data Instruction =
  Load8I Register Word8 |
  Load8 Load8Target Load8Target
  deriving Show
