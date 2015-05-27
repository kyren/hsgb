module Gameboy.Instructions (
  Load8Target(..),
  Instruction(..),
  encodeInstruction,
  decodeInstruction
) where

import Data.Word

data Load8Target =
  Load8A |
  Load8B |
  Load8C |
  Load8D |
  Load8E |
  Load8H |
  Load8L |
  Load8AtHL
  deriving (Show, Eq)

data Instruction =
  NoOp |
  Stop |
  Load8I Load8Target Word8 |
  Load8 Load8Target Load8Target
  deriving (Show, Eq)

encodeInstruction :: Instruction -> [Word8]

encodeInstruction NoOp = [0x0]

encodeInstruction Stop = [0x10, 0x00]

encodeInstruction (Load8I t n) = [code t, n]
  where
    code Load8A = 0x3e
    code Load8B = 0x06
    code Load8C = 0x0e
    code Load8D = 0x16
    code Load8E = 0x1e
    code Load8H = 0x26
    code Load8L = 0x2e
    code Load8AtHL = 0x36

encodeInstruction (Load8 t s) = [code t s]
  where
    code Load8A Load8A = 0x7f
    code Load8A Load8B = 0x78
    code Load8A Load8C = 0x79
    code Load8A Load8D = 0x7a
    code Load8A Load8E = 0x7b
    code Load8A Load8H = 0x7c
    code Load8A Load8L = 0x7d
    code Load8A Load8AtHL = 0x7e
    code Load8B Load8A = 0x47
    code Load8B Load8B = 0x40
    code Load8B Load8C = 0x41
    code Load8B Load8D = 0x42
    code Load8B Load8E = 0x43
    code Load8B Load8H = 0x44
    code Load8B Load8L = 0x45
    code Load8B Load8AtHL = 0x46
    code Load8C Load8A = 0x4f
    code Load8C Load8B = 0x48
    code Load8C Load8C = 0x49
    code Load8C Load8D = 0x4a
    code Load8C Load8E = 0x4b
    code Load8C Load8H = 0x4c
    code Load8C Load8L = 0x4d
    code Load8C Load8AtHL = 0x4e
    code Load8D Load8A = 0x57
    code Load8D Load8B = 0x50
    code Load8D Load8C = 0x51
    code Load8D Load8D = 0x52
    code Load8D Load8E = 0x53
    code Load8D Load8H = 0x54
    code Load8D Load8L = 0x55
    code Load8D Load8AtHL = 0x56
    code Load8E Load8A = 0x5f
    code Load8E Load8B = 0x58
    code Load8E Load8C = 0x59
    code Load8E Load8D = 0x5a
    code Load8E Load8E = 0x5b
    code Load8E Load8H = 0x5c
    code Load8E Load8L = 0x5d
    code Load8E Load8AtHL = 0x5e
    code Load8H Load8A = 0x67
    code Load8H Load8B = 0x60
    code Load8H Load8C = 0x61
    code Load8H Load8D = 0x62
    code Load8H Load8E = 0x63
    code Load8H Load8H = 0x64
    code Load8H Load8L = 0x65
    code Load8H Load8AtHL = 0x66
    code Load8L Load8A = 0x6f
    code Load8L Load8B = 0x68
    code Load8L Load8C = 0x69
    code Load8L Load8D = 0x6a
    code Load8L Load8E = 0x6b
    code Load8L Load8H = 0x6c
    code Load8L Load8L = 0x6d
    code Load8L Load8AtHL = 0x6e
    code Load8AtHL Load8B = 0x70
    code Load8AtHL Load8C = 0x71
    code Load8AtHL Load8D = 0x72
    code Load8AtHL Load8E = 0x73
    code Load8AtHL Load8H = 0x74
    code Load8AtHL Load8L = 0x75
    code _ _ = error "Invalid Instruction"

decodeInstruction :: (Monad m) => m Word8 -> m Instruction
decodeInstruction getWord8 = do
  w <- getWord8
  case w of
    0x00 -> return NoOp
    0x10 -> do
      w2 <- getWord8 
      case w2 of
        0x00 -> return Stop
        _ -> fail "Invalid Instruction"

    0x3e -> Load8I Load8A <$> getWord8
    0x06 -> Load8I Load8B <$> getWord8
    0x0e -> Load8I Load8C <$> getWord8
    0x16 -> Load8I Load8D <$> getWord8
    0x1e -> Load8I Load8E <$> getWord8
    0x26 -> Load8I Load8H <$> getWord8
    0x2e -> Load8I Load8L <$> getWord8
    0x36 -> Load8I Load8AtHL <$> getWord8

    0x70 -> return $ Load8 Load8AtHL Load8B
    0x71 -> return $ Load8 Load8AtHL Load8C
    0x72 -> return $ Load8 Load8AtHL Load8D
    0x73 -> return $ Load8 Load8AtHL Load8E
    0x74 -> return $ Load8 Load8AtHL Load8H
    0x75 -> return $ Load8 Load8AtHL Load8L

    _ -> fail "Invalid Instruction"
