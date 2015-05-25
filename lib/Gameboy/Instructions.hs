module Gameboy.Instructions (
  LoadRegister(..),
  Load8Target(..),
  Instruction(..),
  encodeInstruction,
  decodeInstruction
) where

import Data.Word

data LoadRegister = LoadB | LoadC | LoadD | LoadE | LoadH | LoadL
  deriving (Show, Eq)

data Load8Target = Load8TargetRegister LoadRegister | Load8TargetAtHL
  deriving (Show, Eq)

data Instruction =
  NoOp |
  Stop |
  Load8I LoadRegister Word8 |
  Load8 Load8Target Load8Target
  deriving (Show, Eq)

encodeInstruction :: Instruction -> [Word8]
encodeInstruction NoOp = [0x0]
encodeInstruction Stop = [0x10, 0x00]
encodeInstruction (Load8I lr n) = [code lr, n]
  where
    code LoadB = 0x06
    code LoadC = 0x0e
    code LoadD = 0x16
    code LoadE = 0x1e
    code LoadH = 0x26
    code LoadL = 0x2e
encodeInstruction (Load8 t s) = [code t s]
  where
    athlcode LoadB = 0x70
    athlcode LoadC = 0x71
    athlcode LoadD = 0x72
    athlcode LoadE = 0x73
    athlcode LoadH = 0x74
    athlcode LoadL = 0x75
    code Load8TargetAtHL (Load8TargetRegister lr) = athlcode lr
    code _ _ = error "Unimplemented Load8 instruction encode"

decodeInstruction :: (Monad m) => m Word8 -> m Instruction
decodeInstruction getWord8 = do
  w <- getWord8
  case w of
    0x00 -> return NoOp
    0x06 -> Load8I LoadB <$> getWord8
    0x0e -> Load8I LoadC <$> getWord8
    0x16 -> Load8I LoadD <$> getWord8
    0x1e -> Load8I LoadE <$> getWord8
    0x26 -> Load8I LoadH <$> getWord8
    0x2e -> Load8I LoadL <$> getWord8
    0x70 -> return $ Load8 Load8TargetAtHL (Load8TargetRegister LoadB)
    0x71 -> return $ Load8 Load8TargetAtHL (Load8TargetRegister LoadC)
    0x72 -> return $ Load8 Load8TargetAtHL (Load8TargetRegister LoadD)
    0x73 -> return $ Load8 Load8TargetAtHL (Load8TargetRegister LoadE)
    0x74 -> return $ Load8 Load8TargetAtHL (Load8TargetRegister LoadH)
    0x75 -> return $ Load8 Load8TargetAtHL (Load8TargetRegister LoadL)
    0x10 -> do
      w2 <- getWord8 
      case w2 of
        0x00 -> return Stop
        _ -> fail "Invalid Instruction"
    _ -> fail "Invalid Instruction"
