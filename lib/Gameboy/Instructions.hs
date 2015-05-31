module Gameboy.Instructions (
  Load8Target(..),
  Instruction(..),
  validInstruction,
  encodeInstruction,
  decodeInstruction
) where

import Data.Word
import Data.Maybe

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

-- Not all encodable instructions are valid GB instructions
validInstruction :: Instruction -> Bool
validInstruction = isJust . encodeInstruction

-- If the instruction is a valid GB instruction, returns the opcodes that
-- represent it.
encodeInstruction :: Instruction -> Maybe [Word8]

encodeInstruction NoOp = Just [0x0]

encodeInstruction Stop = Just [0x10, 0x00]

encodeInstruction (Load8I t n) = Just [code t, n]
  where
    code Load8A = 0x3e
    code Load8B = 0x06
    code Load8C = 0x0e
    code Load8D = 0x16
    code Load8E = 0x1e
    code Load8H = 0x26
    code Load8L = 0x2e
    code Load8AtHL = 0x36

encodeInstruction (Load8 t s) =
    case code t s of
      Just c -> Just [c]
      Nothing -> Nothing
  where
    code Load8A Load8A = Just 0x7f
    code Load8A Load8B = Just 0x78
    code Load8A Load8C = Just 0x79
    code Load8A Load8D = Just 0x7a
    code Load8A Load8E = Just 0x7b
    code Load8A Load8H = Just 0x7c
    code Load8A Load8L = Just 0x7d
    code Load8A Load8AtHL = Just 0x7e
    code Load8B Load8A = Just 0x47
    code Load8B Load8B = Just 0x40
    code Load8B Load8C = Just 0x41
    code Load8B Load8D = Just 0x42
    code Load8B Load8E = Just 0x43
    code Load8B Load8H = Just 0x44
    code Load8B Load8L = Just 0x45
    code Load8B Load8AtHL = Just 0x46
    code Load8C Load8A = Just 0x4f
    code Load8C Load8B = Just 0x48
    code Load8C Load8C = Just 0x49
    code Load8C Load8D = Just 0x4a
    code Load8C Load8E = Just 0x4b
    code Load8C Load8H = Just 0x4c
    code Load8C Load8L = Just 0x4d
    code Load8C Load8AtHL = Just 0x4e
    code Load8D Load8A = Just 0x57
    code Load8D Load8B = Just 0x50
    code Load8D Load8C = Just 0x51
    code Load8D Load8D = Just 0x52
    code Load8D Load8E = Just 0x53
    code Load8D Load8H = Just 0x54
    code Load8D Load8L = Just 0x55
    code Load8D Load8AtHL = Just 0x56
    code Load8E Load8A = Just 0x5f
    code Load8E Load8B = Just 0x58
    code Load8E Load8C = Just 0x59
    code Load8E Load8D = Just 0x5a
    code Load8E Load8E = Just 0x5b
    code Load8E Load8H = Just 0x5c
    code Load8E Load8L = Just 0x5d
    code Load8E Load8AtHL = Just 0x5e
    code Load8H Load8A = Just 0x67
    code Load8H Load8B = Just 0x60
    code Load8H Load8C = Just 0x61
    code Load8H Load8D = Just 0x62
    code Load8H Load8E = Just 0x63
    code Load8H Load8H = Just 0x64
    code Load8H Load8L = Just 0x65
    code Load8H Load8AtHL = Just 0x66
    code Load8L Load8A = Just 0x6f
    code Load8L Load8B = Just 0x68
    code Load8L Load8C = Just 0x69
    code Load8L Load8D = Just 0x6a
    code Load8L Load8E = Just 0x6b
    code Load8L Load8H = Just 0x6c
    code Load8L Load8L = Just 0x6d
    code Load8L Load8AtHL = Just 0x6e
    code Load8AtHL Load8B = Just 0x70
    code Load8AtHL Load8C = Just 0x71
    code Load8AtHL Load8D = Just 0x72
    code Load8AtHL Load8E = Just 0x73
    code Load8AtHL Load8H = Just 0x74
    code Load8AtHL Load8L = Just 0x75
    code _ _ = Nothing

-- If the series of opcodes is a valid instruction, returns the instruction.
decodeInstruction :: (Monad m) => m Word8 -> m (Maybe Instruction)
decodeInstruction getWord8 = do
  w <- getWord8
  case w of
    0x00 -> return $ Just NoOp
    0x10 -> do
      w2 <- getWord8 
      case w2 of
        0x00 -> return $ Just Stop
        _ -> return Nothing

    0x3e -> Just <$> Load8I Load8A <$> getWord8
    0x06 -> Just <$> Load8I Load8B <$> getWord8
    0x0e -> Just <$> Load8I Load8C <$> getWord8
    0x16 -> Just <$> Load8I Load8D <$> getWord8
    0x1e -> Just <$> Load8I Load8E <$> getWord8
    0x26 -> Just <$> Load8I Load8H <$> getWord8
    0x2e -> Just <$> Load8I Load8L <$> getWord8
    0x36 -> Just <$> Load8I Load8AtHL <$> getWord8

    0x70 -> return $ Just $ Load8 Load8AtHL Load8B
    0x71 -> return $ Just $ Load8 Load8AtHL Load8C
    0x72 -> return $ Just $ Load8 Load8AtHL Load8D
    0x73 -> return $ Just $ Load8 Load8AtHL Load8E
    0x74 -> return $ Just $ Load8 Load8AtHL Load8H
    0x75 -> return $ Just $ Load8 Load8AtHL Load8L

    _ -> return Nothing
