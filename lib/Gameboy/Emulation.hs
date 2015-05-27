module Gameboy.Emulation (
  step
) where

import Data.Bits
import Data.Word
import Gameboy.CPU
import Gameboy.Instructions

step :: (CPU m, Memory m) => m ()
step = decodeInstruction getNextPC >>= doInstruction

doInstruction :: (CPU m, Memory m) => Instruction -> m ()
doInstruction NoOp = tick 1
doInstruction Stop = stop >> tick 4
doInstruction (Load8I t v) = setLoad8Target t v >> tick 2
doInstruction (Load8 t s) = getLoad8Target s >>= setLoad8Target t 

getLoad8Target :: (CPU m, Memory m) => Load8Target -> m Word8
getLoad8Target Load8A = getARegister
getLoad8Target Load8B = getBRegister
getLoad8Target Load8C = getCRegister
getLoad8Target Load8D = getDRegister
getLoad8Target Load8E = getERegister
getLoad8Target Load8H = getHRegister
getLoad8Target Load8L = getLRegister
getLoad8Target Load8AtHL = do
  h <- getHRegister
  l <- getLRegister
  getMemory (makeWord h l)

setLoad8Target :: (CPU m, Memory m) => Load8Target -> Word8 -> m ()
setLoad8Target Load8A = setARegister
setLoad8Target Load8B = setBRegister
setLoad8Target Load8C = setCRegister
setLoad8Target Load8D = setDRegister
setLoad8Target Load8E = setERegister
setLoad8Target Load8H = setHRegister
setLoad8Target Load8L = setLRegister
setLoad8Target Load8AtHL = \v -> do
  h <- getHRegister
  l <- getLRegister
  setMemory (makeWord h l) v

makeWord :: Word8 -> Word8 -> Word16
makeWord h l = shift (fromIntegral h) 8 .|. fromIntegral l

getNextPC :: (CPU m, Memory m) => m Word8
getNextPC = do
  pc <- getProgramCounter
  setProgramCounter (pc + 1)
  getMemory pc

{-
doOp :: (CPU m, Memory m) => Word8 -> m ()
doOp 0x00 = noop
doOp 0x01 = load_rxx_nn BCRegister
doOp 0x02 = load_mrxx_rx BCRegister ARegister
doOp 0x03 = inc_rxx BCRegister
doOp 0x04 = inc_rx BRegister
doOp 0x05 = dec_rx BRegister
doOp 0x06 = load_rx_n BRegister
doOp 0x07 = rlca
doOp 0x08 = load_mnn_sp
doOp 0xc3 = jump_nn
doOp _ = fail "Invalid Instruction"

noop :: (CPU m, Memory m) => m ()
noop = tick 1

load_rxx_nn :: (CPU m, Memory m) => Register16 -> m ()
load_rxx_nn r16 = do
  getNextPC16 >>= setRegister16 r16
  tick 3

load_mrxx_rx :: (CPU m, Memory m) => Register16 -> Register -> m ()
load_mrxx_rx r16 sr = do
  addr <- getRegister16 r16
  v <- getRegister sr
  setMemory addr v
  tick 2

load_rx_rx :: CPU m => Register -> Register -> m ()
load_rx_rx sourceReg targetReg = do
  getRegister sourceReg >>= setRegister targetReg
  tick 1

load_rx_mhl :: (CPU m, Memory m) => Register -> m ()
load_rx_mhl targetReg = do
  getRegister16 HLRegister >>= getMemory >>= setRegister targetReg
  tick 2

load_mhl_rx :: (CPU m, Memory m) => Register -> m ()
load_mhl_rx sourceReg = do
  addr <- getRegister16 HLRegister
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
  addr <- getRegister16 HLRegister
  setMemory addr v
  tick 2

load_mxx_rx :: (CPU m, Memory m) => Register16 -> Register -> m ()
load_mxx_rx r16 sourceReg = do
  addr <- getRegister16 r16
  v <- getRegister sourceReg
  setMemory addr v
  tick 2

load_mnn_rx :: (CPU m, Memory m) => Register -> m ()
load_mnn_rx sourceReg = do
  addr <- getNextPC16
  val <- getRegister sourceReg
  setMemory addr val
  tick 4

load_rx_mxx :: (CPU m, Memory m) => Register -> Register16 -> m ()
load_rx_mxx targetReg mr16 = do
  addr <- getRegister16 mr16
  val <- getMemory addr
  setRegister targetReg val
  tick 2

load_rx_mnn :: (CPU m, Memory m) => Register -> m ()
load_rx_mnn targetReg = do
  addr <- getNextPC16
  val <- getMemory addr
  setRegister targetReg val
  tick 4

load_mnn_sp :: (CPU m, Memory m) => m ()
load_mnn_sp = do
  addr <- getNextPC16
  sp <- getStackPointer
  setMemory16 addr sp
  tick 5

inc_rx :: CPU m => Register -> m ()
inc_rx r = do
  v <- getRegister r
  let inc = v + 1
  setRegister r inc
  setFlag Zero (inc == 0)
  setFlag Operation False
  setFlag HalfCarry (inc .&. 0x0F == 0)
  tick 1

inc_rxx :: CPU m => Register16 -> m ()
inc_rxx r16 = do
  val <- getRegister16 r16
  setRegister16 r16 (val + 1)
  tick 2

dec_rx :: CPU m => Register -> m ()
dec_rx r = do
  v <- getRegister r
  let dec = v - 1
  setRegister r dec
  setFlag Zero (dec == 0)
  setFlag Operation True
  setFlag HalfCarry (dec .&. 0x0F == 0x0F)
  tick 1

dec_rxx :: CPU m => Register16 -> m ()
dec_rxx r16 = do
  val <- getRegister16 r16
  setRegister16 r16 (val - 1)
  tick 2

rlca :: CPU m => m ()
rlca = do
  val <- getRegister ARegister
  let rval = rotateL val 1
  setRegister ARegister rval
  setFlag Carry (testBit val 7)
  tick 1

pushr :: (CPU m, Memory m) => Register16 -> m ()
pushr r16 = do
  val <- getRegister16 r16
  pushStack16 val
  tick 3

popr :: (CPU m, Memory m) => Register16 -> m ()
popr r16 = do
  val <- popStack16
  setRegister16 r16 val
  tick 3

jump_nn :: (CPU m, Memory m) => m ()
jump_nn = do
  getNextPC16 >>= setProgramCounter
  tick 3

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

highByte :: Word16 -> Word8
highByte w = fromIntegral (shift w (-8))

lowByte :: Word16 -> Word8
lowByte = fromIntegral
-}
