module Gameboy.Emulation where

import Data.Word
import Data.Bits
import Gameboy.CPU

{-# ANN module "HLint: ignore Use camelCase" #-} 

step :: (CPU m, Memory m) => m ()
step = getNextPC >>= doOp

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
