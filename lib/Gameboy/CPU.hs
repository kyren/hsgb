module Gameboy.CPU where

import Data.Word
import Data.Bits

class Monad m => Memory m where
  getMemory :: Word16 -> m Word8
  setMemory :: Word16 -> Word8 -> m ()

data Register = ARegister | BRegister | CRegister | DRegister | ERegister | HRegister | LRegister | FRegister

class Monad m => CPU m where
  getRegister :: Register -> m Word8
  setRegister :: Register -> Word8 -> m ()
  getProgramCounter :: m Word16
  setProgramCounter :: Word16 -> m ()
  getStackPointer :: m Word16
  setStackPointer :: Word16 -> m ()
  tick :: Int -> m ()

makeWord :: Word8 -> Word8 -> Word16
makeWord h l = shift (fromIntegral h) 8 .|. fromIntegral l

highByte :: Word16 -> Word8
highByte w = fromIntegral (shift w (-8))

lowByte :: Word16 -> Word8
lowByte = fromIntegral

getRegister16 :: CPU m => Register -> Register -> m Word16
getRegister16 regh regl = do
  high <- getRegister regh
  low <- getRegister regl
  return $ makeWord high low

setRegister16 :: CPU m => Register -> Register -> Word16 -> m ()
setRegister16 regh regl val = do
  setRegister regh (highByte val)
  setRegister regl (lowByte val)

getMemory16 :: Memory m => Word16 -> m Word16
getMemory16 addr = do
  low <- getMemory addr
  high <- getMemory (addr + 1)
  return $ makeWord high low

setMemory16 :: Memory m => Word16 -> Word16 -> m ()
setMemory16 addr val = do
  setMemory addr (lowByte val)
  setMemory (addr + 1) (highByte val)

getNextPC :: (CPU m, Memory m) => m Word8
getNextPC = do
  pc <- getProgramCounter
  setProgramCounter (pc + 1)
  getMemory pc

getNextPC16 :: (CPU m, Memory m) => m Word16
getNextPC16 = do
  pc <- getProgramCounter
  setProgramCounter (pc + 2)
  getMemory16 pc

pushStack :: (CPU m, Memory m) => Word8 -> m ()
pushStack val = do
  sp <- getStackPointer
  setStackPointer (sp - 1)
  setMemory (sp - 1) val

pushStack16 :: (CPU m, Memory m) => Word16 -> m ()
pushStack16 val = do
  pushStack (highByte val)
  pushStack (lowByte val)

popStack :: (CPU m, Memory m) => m Word8
popStack = do
  sp <- getStackPointer
  val <- getMemory sp
  setStackPointer (sp - 1)
  return val

popStack16 :: (CPU m, Memory m) => m Word16
popStack16 = do
  l <- popStack
  h <- popStack
  return $ makeWord h l
