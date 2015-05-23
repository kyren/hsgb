module Gameboy.Instructions where

import Data.Word

data LoadRegister = LoadB | LoadC | LoadD | LoadE | LoadH | LoadL
  deriving (Show, Eq)

data Load8Target = Load8TargetRegister LoadRegister | Load8TargetAtHL
  deriving (Show, Eq)

data Instruction =
  Load8I LoadRegister Word8 |
  Load8 Load8Target Load8Target
  deriving (Show, Eq)

encodeInstruction :: Instruction -> [Word8]
encodeInstruction (Load8I lr n) = [code lr, n]
  where
    code LoadB = 0x06
    code LoadC = 0x0e
    code LoadD = 0x16
    code LoadE = 0x1e
    code LoadH = 0x26
    code LoadL = 0x2e
encodeInstruction _ = undefined

decodeInstruction :: (Monad m) => m Word8 -> m (Maybe Instruction)
decodeInstruction getWord8 = do
  w <- getWord8
  case w of
    0x06 -> Just <$> Load8I LoadB <$> getWord8
    0x0e -> Just <$> Load8I LoadC <$> getWord8
    0x16 -> Just <$> Load8I LoadD <$> getWord8
    0x1e -> Just <$> Load8I LoadE <$> getWord8
    0x26 -> Just <$> Load8I LoadH <$> getWord8
    0x2e -> Just <$> Load8I LoadL <$> getWord8
    _ -> return Nothing
