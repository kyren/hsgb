module Gameboy.Instructions where

import Data.Word
import Control.Monad
import Gameboy.CPU

{-# ANN module "HLint: ignore Use camelCase" #-} 

step :: (CPU m, Memory m) => m ()
step = getNextPC >>= doOp

doOp :: (CPU m, Memory m) => Word8 -> m ()
doOp 0x0 = return ()
doOp 0x1 = load_rxx_nn BRegister CRegister
doOp 0x2 = load_mrxx_rx BRegister CRegister ARegister
doOp 0x3 = inc_rxx BRegister CRegister
doOp _ = fail "Invalid Instruction"

load_rxx_nn :: (CPU m, Memory m) => Register -> Register -> m ()
load_rxx_nn rh rl = do
  getNextPC16 >>= setRegister16 rh rl
  tick 3

load_mrxx_rx :: (CPU m, Memory m) => Register -> Register -> Register -> m ()
load_mrxx_rx rh rl sr = do
  addr <- getRegister16 rh rl
  v <- getRegister sr
  setMemory addr v
  tick 2

inc_rxx :: CPU m => Register -> Register -> m ()
inc_rxx rh rl = do
  hval <- getRegister rh
  lval <- getRegister rl
  let incval = lval + 1
  setRegister rl incval
  when (incval == 0) $ setRegister rh (hval + 1)

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