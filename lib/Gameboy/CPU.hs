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
  high <- getMemory addr
  low <- getMemory (addr + 1)
  return $ makeWord high low

setMemory16 :: Memory m => Word16 -> Word16 -> m ()
setMemory16 addr val = do
  setMemory addr (highByte val)
  setMemory (addr + 1) (lowByte val)

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

load_rx_rx :: CPU m => Register -> Register -> m ()
load_rx_rx sourceReg targetReg = do
  getRegister sourceReg >>= setRegister targetReg
  tick 1

load_rx_mhl :: (CPU m, Memory m) => Register -> m ()
load_rx_mhl targetReg = do
  getRegister16 HRegister LRegister >>= getMemory >>= setRegister targetReg
  tick 2

load_mhl_rx :: (CPU m, Memory m) => Register -> m ()
load_mhl_rx sourceReg = do
  addr <- getRegister16 HRegister LRegister
  val <- getRegister sourceReg
  setMemory addr val
  tick 2

load_rx_n :: (CPU m, Memory m) => Register -> m ()
load_rx_n targetReg = do
  getNextPC >>= setRegister targetReg
  tick 2

load_mhl_n :: (CPU m, Memory m) => m ()
load_mhl_n = do
  v <- getNextPC
  addr <- getRegister16 HRegister LRegister
  setMemory addr v
  tick 2

load_mxx_rx :: (CPU m, Memory m) => Register -> Register -> Register -> m ()
load_mxx_rx hReg lReg sourceReg = do
  addr <- getRegister16 hReg lReg
  v <- getRegister sourceReg
  setMemory addr v
  tick 2

load_mnn_rx :: (CPU m, Memory m) => Register -> m ()
load_mnn_rx sourceReg = do
  addr <- getNextPC16
  val <- getRegister sourceReg
  setMemory addr val
  tick 4

load_rx_mxx :: (CPU m, Memory m) => Register -> Register -> Register -> m ()
load_rx_mxx targetReg memReg1 memReg2 = do
  addr <- getRegister16 memReg1 memReg2
  val <- getMemory addr
  setRegister targetReg val
  tick 2

load_rx_mnn :: (CPU m, Memory m) => Register -> m ()
load_rx_mnn targetReg = do
  addr <- getNextPC16
  val <- getMemory addr
  setRegister targetReg val
  tick 4

pushr :: (CPU m, Memory m) => Register -> Register -> m ()
pushr reg1 reg2 = do
  val <- getRegister16 reg1 reg2
  pushStack16 val
  tick 3

popr :: (CPU m, Memory m) => Register -> Register -> m ()
popr reg1 reg2 = do
  val <- popStack16
  setRegister reg1 (highByte val)
  setRegister reg2 (lowByte val)
  tick 3
