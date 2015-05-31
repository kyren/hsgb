module Gameboy.Instructions (
  Load8Part(..),
  Instruction(..),
  validInstruction,
  encodeInstruction,
  decodeInstruction
) where

import Data.Word
import Data.Maybe
import Gameboy.Util

data Load8Part =
  Load8A |
  Load8B |
  Load8C |
  Load8D |
  Load8E |
  Load8H |
  Load8L |
  Load8AtBC |
  Load8AtDE |
  Load8AtHL |
  Load8AtNN Word16 |
  Load8I Word8
  deriving (Show, Eq)

data Instruction =
  NoOp |
  Stop |
  Load8 Load8Part Load8Part
  deriving (Show, Eq)

-- Not all encodable instructions are valid GB instructions
validInstruction :: Instruction -> Bool
validInstruction = isJust . encodeInstruction

-- If the instruction is a valid GB instruction, returns the opcodes that
-- represent it.
encodeInstruction :: Instruction -> Maybe [Word8]

encodeInstruction NoOp = Just [0x0]

encodeInstruction Stop = Just [0x10, 0x00]

encodeInstruction (Load8 t s) = code t s
  where
    code Load8A (Load8I n) = Just [0x3e, n]
    code Load8B (Load8I n) = Just [0x06, n]
    code Load8C (Load8I n) = Just [0x0e, n]
    code Load8D (Load8I n) = Just [0x16, n]
    code Load8E (Load8I n) = Just [0x1e, n]
    code Load8H (Load8I n) = Just [0x26, n]
    code Load8L (Load8I n) = Just [0x2e, n]
    code Load8AtHL (Load8I n) = Just [0x36, n]

    code Load8A Load8A = Just [0x7f]
    code Load8A Load8B = Just [0x78]
    code Load8A Load8C = Just [0x79]
    code Load8A Load8D = Just [0x7a]
    code Load8A Load8E = Just [0x7b]
    code Load8A Load8H = Just [0x7c]
    code Load8A Load8L = Just [0x7d]
    code Load8A Load8AtBC = Just [0x0a]
    code Load8A Load8AtDE = Just [0x1a]
    code Load8A Load8AtHL = Just [0x7e]
    code Load8A (Load8AtNN nn) = Just [0xfa, lowByte nn, highByte nn]

    code Load8AtBC Load8A = Just [0x02]
    code Load8AtDE Load8A = Just [0x12]
    code Load8AtHL Load8A = Just [0x77]
    code (Load8AtNN nn) Load8A = Just [0xea, lowByte nn, highByte nn]

    code Load8B Load8A = Just [0x47]
    code Load8B Load8B = Just [0x40]
    code Load8B Load8C = Just [0x41]
    code Load8B Load8D = Just [0x42]
    code Load8B Load8E = Just [0x43]
    code Load8B Load8H = Just [0x44]
    code Load8B Load8L = Just [0x45]
    code Load8B Load8AtHL = Just [0x46]

    code Load8C Load8A = Just [0x4f]
    code Load8C Load8B = Just [0x48]
    code Load8C Load8C = Just [0x49]
    code Load8C Load8D = Just [0x4a]
    code Load8C Load8E = Just [0x4b]
    code Load8C Load8H = Just [0x4c]
    code Load8C Load8L = Just [0x4d]
    code Load8C Load8AtHL = Just [0x4e]

    code Load8D Load8A = Just [0x57]
    code Load8D Load8B = Just [0x50]
    code Load8D Load8C = Just [0x51]
    code Load8D Load8D = Just [0x52]
    code Load8D Load8E = Just [0x53]
    code Load8D Load8H = Just [0x54]
    code Load8D Load8L = Just [0x55]
    code Load8D Load8AtHL = Just [0x56]

    code Load8E Load8A = Just [0x5f]
    code Load8E Load8B = Just [0x58]
    code Load8E Load8C = Just [0x59]
    code Load8E Load8D = Just [0x5a]
    code Load8E Load8E = Just [0x5b]
    code Load8E Load8H = Just [0x5c]
    code Load8E Load8L = Just [0x5d]
    code Load8E Load8AtHL = Just [0x5e]

    code Load8H Load8A = Just [0x67]
    code Load8H Load8B = Just [0x60]
    code Load8H Load8C = Just [0x61]
    code Load8H Load8D = Just [0x62]
    code Load8H Load8E = Just [0x63]
    code Load8H Load8H = Just [0x64]
    code Load8H Load8L = Just [0x65]
    code Load8H Load8AtHL = Just [0x66]

    code Load8L Load8A = Just [0x6f]
    code Load8L Load8B = Just [0x68]
    code Load8L Load8C = Just [0x69]
    code Load8L Load8D = Just [0x6a]
    code Load8L Load8E = Just [0x6b]
    code Load8L Load8H = Just [0x6c]
    code Load8L Load8L = Just [0x6d]
    code Load8L Load8AtHL = Just [0x6e]

    code Load8AtHL Load8B = Just [0x70]
    code Load8AtHL Load8C = Just [0x71]
    code Load8AtHL Load8D = Just [0x72]
    code Load8AtHL Load8E = Just [0x73]
    code Load8AtHL Load8H = Just [0x74]
    code Load8AtHL Load8L = Just [0x75]

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

    0x3e -> Just <$> Load8 Load8A <$> Load8I <$> getWord8
    0x06 -> Just <$> Load8 Load8B <$> Load8I <$> getWord8
    0x0e -> Just <$> Load8 Load8C <$> Load8I <$> getWord8
    0x16 -> Just <$> Load8 Load8D <$> Load8I <$> getWord8
    0x1e -> Just <$> Load8 Load8E <$> Load8I <$> getWord8
    0x26 -> Just <$> Load8 Load8H <$> Load8I <$> getWord8
    0x2e -> Just <$> Load8 Load8L <$> Load8I <$> getWord8
    0x36 -> Just <$> Load8 Load8AtHL <$> Load8I <$> getWord8

    0x7f -> return $ Just $ Load8 Load8A Load8A
    0x78 -> return $ Just $ Load8 Load8A Load8B
    0x79 -> return $ Just $ Load8 Load8A Load8C
    0x7a -> return $ Just $ Load8 Load8A Load8D
    0x7b -> return $ Just $ Load8 Load8A Load8E
    0x7c -> return $ Just $ Load8 Load8A Load8H
    0x7d -> return $ Just $ Load8 Load8A Load8L

    0x0a -> return $ Just $ Load8 Load8A Load8AtBC
    0x1a -> return $ Just $ Load8 Load8A Load8AtDE
    0x7e -> return $ Just $ Load8 Load8A Load8AtHL

    0xfa -> do
      lb <- getWord8
      hb <- getWord8
      return $ Just $ Load8 Load8A (Load8AtNN (makeWord lb hb))

    0x02 -> return $ Just $ Load8 Load8AtBC Load8A
    0x12 -> return $ Just $ Load8 Load8AtDE Load8A
    0x77 -> return $ Just $ Load8 Load8AtHL Load8A

    0xea -> do
      lb <- getWord8
      hb <- getWord8
      return $ Just $ Load8 (Load8AtNN (makeWord lb hb)) Load8A

    0x47 -> return $ Just $ Load8 Load8B Load8A
    0x40 -> return $ Just $ Load8 Load8B Load8B
    0x41 -> return $ Just $ Load8 Load8B Load8C
    0x42 -> return $ Just $ Load8 Load8B Load8D
    0x43 -> return $ Just $ Load8 Load8B Load8E
    0x44 -> return $ Just $ Load8 Load8B Load8H
    0x45 -> return $ Just $ Load8 Load8B Load8L
    0x46 -> return $ Just $ Load8 Load8B Load8AtHL

    0x4f -> return $ Just $ Load8 Load8C Load8A
    0x48 -> return $ Just $ Load8 Load8C Load8B
    0x49 -> return $ Just $ Load8 Load8C Load8C
    0x4a -> return $ Just $ Load8 Load8C Load8D
    0x4b -> return $ Just $ Load8 Load8C Load8E
    0x4c -> return $ Just $ Load8 Load8C Load8H
    0x4d -> return $ Just $ Load8 Load8C Load8L
    0x4e -> return $ Just $ Load8 Load8C Load8AtHL

    0x57 -> return $ Just $ Load8 Load8D Load8A
    0x50 -> return $ Just $ Load8 Load8D Load8B
    0x51 -> return $ Just $ Load8 Load8D Load8C
    0x52 -> return $ Just $ Load8 Load8D Load8D
    0x53 -> return $ Just $ Load8 Load8D Load8E
    0x54 -> return $ Just $ Load8 Load8D Load8H
    0x55 -> return $ Just $ Load8 Load8D Load8L
    0x56 -> return $ Just $ Load8 Load8D Load8AtHL

    0x5f -> return $ Just $ Load8 Load8E Load8A
    0x58 -> return $ Just $ Load8 Load8E Load8B
    0x59 -> return $ Just $ Load8 Load8E Load8C
    0x5a -> return $ Just $ Load8 Load8E Load8D
    0x5b -> return $ Just $ Load8 Load8E Load8E
    0x5c -> return $ Just $ Load8 Load8E Load8H
    0x5d -> return $ Just $ Load8 Load8E Load8L
    0x5e -> return $ Just $ Load8 Load8E Load8AtHL

    0x67 -> return $ Just $ Load8 Load8H Load8A
    0x60 -> return $ Just $ Load8 Load8H Load8B
    0x61 -> return $ Just $ Load8 Load8H Load8C
    0x62 -> return $ Just $ Load8 Load8H Load8D
    0x63 -> return $ Just $ Load8 Load8H Load8E
    0x64 -> return $ Just $ Load8 Load8H Load8H
    0x65 -> return $ Just $ Load8 Load8H Load8L
    0x66 -> return $ Just $ Load8 Load8H Load8AtHL

    0x6f -> return $ Just $ Load8 Load8L Load8A
    0x68 -> return $ Just $ Load8 Load8L Load8B
    0x69 -> return $ Just $ Load8 Load8L Load8C
    0x6a -> return $ Just $ Load8 Load8L Load8D
    0x6b -> return $ Just $ Load8 Load8L Load8E
    0x6c -> return $ Just $ Load8 Load8L Load8H
    0x6d -> return $ Just $ Load8 Load8L Load8L
    0x6e -> return $ Just $ Load8 Load8L Load8AtHL

    0x70 -> return $ Just $ Load8 Load8AtHL Load8B
    0x71 -> return $ Just $ Load8 Load8AtHL Load8C
    0x72 -> return $ Just $ Load8 Load8AtHL Load8D
    0x73 -> return $ Just $ Load8 Load8AtHL Load8E
    0x74 -> return $ Just $ Load8 Load8AtHL Load8H
    0x75 -> return $ Just $ Load8 Load8AtHL Load8L

    _ -> return Nothing
