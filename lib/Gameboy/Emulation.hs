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

setMemory16 :: (Memory m) => Word16 -> Word16 -> m ()
setMemory16 addr nn = do
  setMemory addr (highByte nn)
  setMemory (addr + 1) (lowByte nn)

getNextPC :: (CPU m, Memory m) => m Word8
getNextPC = do
  pc <- getProgramCounter
  if pc == maxBound
    then fail "PC wrapped"
    else do
      setProgramCounter (pc + 1)
      getMemory pc

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

getBC :: (CPU m) => m Word16
getBC = do
  h <- getBRegister
  l <- getCRegister
  return $ makeWord l h

setBC :: (CPU m) => Word16 -> m ()
setBC nn = do
  setBRegister $ highByte nn
  setCRegister $ lowByte nn

getDE :: (CPU m) => m Word16
getDE = do
  h <- getDRegister
  l <- getERegister
  return $ makeWord l h

setDE :: (CPU m) => Word16 -> m ()
setDE nn = do
  setDRegister $ highByte nn
  setERegister $ lowByte nn

getHL :: (CPU m) => m Word16
getHL = do
  h <- getHRegister
  l <- getLRegister
  return $ makeWord l h

setHL :: (CPU m) => Word16 -> m ()
setHL nn = do
  setHRegister $ highByte nn
  setLRegister $ lowByte nn

getAtBC :: (CPU m, Memory m) => m Word8
getAtBC = getBC >>= getMemory

setAtBC :: (CPU m, Memory m) => Word8 -> m ()
setAtBC n = getBC >>= (`setMemory` n)

getAtDE :: (CPU m, Memory m) => m Word8
getAtDE = getDE >>= getMemory

setAtDE :: (CPU m, Memory m) => Word8 -> m ()
setAtDE n = getDE >>= (`setMemory` n)

getAtHL :: (CPU m, Memory m) => m Word8
getAtHL = getHL >>= getMemory

setAtHL :: (CPU m, Memory m) => Word8 -> m ()
setAtHL n = getHL >>= (`setMemory` n)

getAtC :: (CPU m, Memory m) => m Word8
getAtC = do
  c <- getCRegister
  getMemory (makeWord c 0xff)

setAtC :: (CPU m, Memory m) => Word8 -> m ()
setAtC v = do
  c <- getCRegister
  setMemory (makeWord c 0xff) v

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

doInstruction LD_A_ATC = do
  v <- getAtC
  setARegister v
  tick 8

doInstruction LD_A_ATBC = do
  v <- getAtBC
  setARegister v
  tick 8

doInstruction LD_A_ATDE = do
  v <- getAtDE
  setARegister v
  tick 8

doInstruction (LD_A_ATNN nn) = do
  v <- getMemory nn
  setARegister v
  tick 16

doInstruction LD_ATC_A = do
  v <- getARegister
  setAtC v
  tick 8

doInstruction LD_ATBC_A = do
  v <- getARegister
  setAtBC v
  tick 8

doInstruction LD_ATDE_A = do
  v <- getARegister
  setAtDE v
  tick 8

doInstruction (LD_ATNN_A nn) = do
  v <- getARegister
  setMemory nn v
  tick 16

doInstruction LDD_A_ATHL = do
  hl <- getHL
  v <- getMemory hl
  setARegister v
  setHL (hl - 1)
  tick 8

doInstruction LDD_ATHL_A = do
  hl <- getHL
  v <- getARegister
  setMemory hl v
  setHL (hl - 1)
  tick 8

doInstruction LDI_A_ATHL = do
  hl <- getHL
  v <- getMemory hl
  setARegister v
  setHL (hl + 1)
  tick 8

doInstruction LDI_ATHL_A = do
  hl <- getHL
  v <- getARegister
  setMemory hl v
  setHL (hl + 1)
  tick 8

doInstruction (LDH_A_ATN n) = do
  v <- getMemory (makeWord n 0xff)
  setARegister v
  tick 12

doInstruction (LDH_ATN_A n) = do
  v <- getARegister
  setMemory (makeWord n 0xff) v
  tick 12

doInstruction (LD_BC_NN nn) = do
  setBC nn
  tick 12

doInstruction (LD_DE_NN nn) = do
  setDE nn
  tick 12

doInstruction (LD_HL_NN nn) = do
  setHL nn
  tick 12

doInstruction (LD_SP_NN nn) = do
  setStackPointer nn
  tick 12

doInstruction LD_SP_HL = do
  sp <- getStackPointer
  setHL sp
  tick 8

doInstruction (LDHL_SP_N n) = do
  sp <- getStackPointer
  setHL (sp + fromIntegral n)
  tick 12

doInstruction (LD_ATNN_SP nn) = do
  sp <- getStackPointer
  setMemory16 nn sp
  tick 20

doInstruction NOP = tick 4
doInstruction STOP = stop >> tick 16

doInstruction _ = error "instruction step unimplemented!"
