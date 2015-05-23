module Gameboy.CPU where

import Data.Word
import Data.Bits

class Monad m => Memory m where
  getMemory :: Word16 -> m Word8
  setMemory :: Word16 -> Word8 -> m ()

data Register = ARegister | BRegister | CRegister | DRegister | ERegister | HRegister | LRegister | FRegister
data Register16 = AFRegister | BCRegister | DERegister | HLRegister
data Flag = Zero | Operation | HalfCarry | Carry

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

flagBit :: Flag -> Int
flagBit Zero = 7
flagBit Operation = 6
flagBit HalfCarry = 5
flagBit Carry = 4

decomposeRegister16 :: Register16 -> (Register, Register)
decomposeRegister16 AFRegister = (ARegister, FRegister)
decomposeRegister16 BCRegister = (BRegister, CRegister)
decomposeRegister16 DERegister = (DRegister, ERegister)
decomposeRegister16 HLRegister = (HRegister, LRegister)

getRegister16 :: CPU m => Register16 -> m Word16
getRegister16 reg = do
  let (regh, regl) = decomposeRegister16 reg
  high <- getRegister regh
  low <- getRegister regl
  return $ makeWord high low

setRegister16 :: CPU m => Register16 -> Word16 -> m ()
setRegister16 reg val = do
  let (regh, regl) = decomposeRegister16 reg
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

getFlag :: (CPU m) => Flag -> m Bool
getFlag fl = do
  freg <- getRegister FRegister
  return $ testBit freg (flagBit fl)

setFlag :: (CPU m) => Flag -> Bool -> m ()
setFlag flag state = do
  freg <- getRegister FRegister
  let fbit = flagBit flag
  let ufreg = if state then setBit freg fbit else clearBit freg fbit
  setRegister FRegister ufreg
  return ()
