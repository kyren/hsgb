module Gameboy.Emulation (
  step
) where

import Data.Word
import Gameboy.Util
import Gameboy.CPU
import Gameboy.Instructions

step :: (CPU m, Memory m) => m ()
step = do
  mi <- decodeInstruction getNextPC
  case mi of
    Just i -> doInstruction i
    Nothing -> fail "invalid opcode"

getNextPC :: (CPU m, Memory m) => m (Maybe Word8)
getNextPC = do
  pc <- getProgramCounter
  if pc == maxBound
    then return Nothing
    else do
      setProgramCounter (pc + 1)
      Just <$> getMemory pc

getRegister :: (CPU m) => Register -> m Word8
getRegister ARegister = getARegister
getRegister BRegister = getBRegister
getRegister CRegister = getCRegister
getRegister DRegister = getDRegister
getRegister ERegister = getERegister
getRegister HRegister = getHRegister
getRegister LRegister = getLRegister

setRegister :: (CPU m) => Register -> Word8 -> m ()
setRegister ARegister = setARegister
setRegister BRegister = setBRegister
setRegister CRegister = setCRegister
setRegister DRegister = setDRegister
setRegister ERegister = setERegister
setRegister HRegister = setHRegister
setRegister LRegister = setLRegister

getAtHL :: (CPU m, Memory m) => m Word8
getAtHL = do
  h <- getHRegister
  l <- getLRegister
  getMemory (makeWord l h)

setAtHL :: (CPU m, Memory m) => Word8 -> m ()
setAtHL v = do
  h <- getHRegister
  l <- getLRegister
  setMemory (makeWord l h) v

doInstruction :: (CPU m, Memory m) => Instruction -> m ()

doInstruction (LD_R_R t s) = do
  getRegister s >>= setRegister t
  tick 4

doInstruction (LD_R_N t n) = do
  setRegister t n
  tick 8

doInstruction (LD_R_ATHL t) = do
  getAtHL >>= setRegister t
  tick 8

doInstruction (LD_ATHL_R s) = do
  getRegister s >>= setAtHL
  tick 8

doInstruction (LD_ATHL_N n) = do
  setAtHL n
  tick 12

doInstruction NOP = tick 4
doInstruction STOP = stop >> tick 16

doInstruction _ = error "instruction step unimplemented!"
