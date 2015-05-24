module Gameboy.Instructions where

import Data.Word

data NoOp

data LoadRegister = LoadB | LoadC | LoadD | LoadE | LoadH | LoadL
  deriving (Show, Eq)

data Load8Target = Load8TargetRegister LoadRegister | Load8TargetAtHL
  deriving (Show, Eq)

data Instruction =
  NoOp |
  Load8I LoadRegister Word8 |
  Load8 Load8Target Load8Target
  deriving (Show, Eq)

encodeInstruction :: Instruction -> [Word8]
encodeInstruction NoOp = [0x0]
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

decodeInstruction :: (Monad m) => m Word8 -> m (Maybe Instruction)
decodeInstruction getWord8 = do
  w <- getWord8
  case w of
    0x00 -> return $ Just NoOp
    0x06 -> Just <$> Load8I LoadB <$> getWord8
    0x0e -> Just <$> Load8I LoadC <$> getWord8
    0x16 -> Just <$> Load8I LoadD <$> getWord8
    0x1e -> Just <$> Load8I LoadE <$> getWord8
    0x26 -> Just <$> Load8I LoadH <$> getWord8
    0x2e -> Just <$> Load8I LoadL <$> getWord8
    0x70 -> return $ Just $ Load8 Load8TargetAtHL (Load8TargetRegister LoadB)
    0x71 -> return $ Just $ Load8 Load8TargetAtHL (Load8TargetRegister LoadC)
    0x72 -> return $ Just $ Load8 Load8TargetAtHL (Load8TargetRegister LoadD)
    0x73 -> return $ Just $ Load8 Load8TargetAtHL (Load8TargetRegister LoadE)
    0x74 -> return $ Just $ Load8 Load8TargetAtHL (Load8TargetRegister LoadH)
    0x75 -> return $ Just $ Load8 Load8TargetAtHL (Load8TargetRegister LoadL)
    _ -> return Nothing
