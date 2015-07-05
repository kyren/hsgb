module Gameboy.Emulation (
  step
) where

import Data.Word
import Data.Bits
import Gameboy.Util
import Gameboy.CPU
import Gameboy.Instructions

step :: (CPU m, Memory m) => m ()
step = do
  mi <- decodeInstruction getNextPC
  case mi of
    Just i -> doInstruction i
    Nothing -> fail "invalid opcode"

data Flag
  = ZFlag
  | NFlag
  | HFlag
  | CFlag

flagBit :: Flag -> Int
flagBit ZFlag = 7
flagBit NFlag = 6
flagBit HFlag = 5
flagBit CFlag = 4

setMemory16 :: (Memory m) => Word16 -> Word16 -> m ()
setMemory16 addr nn = do
  setMemory addr (highByte nn)
  setMemory (addr + 1) (lowByte nn)

getMemory16 :: (Memory m) => Word16 -> m Word16
getMemory16 addr = do
  h <- getMemory addr
  l <- getMemory (addr + 1)
  return $ makeWord16 l h

getNextPC :: (CPU m, Memory m) => m Word8
getNextPC = do
  pc <- getProgramCounter
  if pc == maxBound
    then fail "PC wrapped"
    else do
      setProgramCounter (pc + 1)
      getMemory pc

pushStack16 :: (CPU m, Memory m) => Word16 -> m ()
pushStack16 nn = do
  sp <- getStackPointer
  setMemory16 sp nn
  setStackPointer (sp - 2)

popStack16 :: (CPU m, Memory m) => m Word16
popStack16 = do
  sp <- getStackPointer
  nn <- getMemory16 sp
  setStackPointer (sp + 2)
  return nn

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

getAF :: (CPU m) => m Word16
getAF = do
  h <- getARegister
  l <- getFRegister
  return $ makeWord16 l h

setAF :: (CPU m) => Word16 -> m ()
setAF nn = do
  setARegister $ highByte nn
  setFRegister $ lowByte nn

getBC :: (CPU m) => m Word16
getBC = do
  h <- getBRegister
  l <- getCRegister
  return $ makeWord16 l h

setBC :: (CPU m) => Word16 -> m ()
setBC nn = do
  setBRegister $ highByte nn
  setCRegister $ lowByte nn

getDE :: (CPU m) => m Word16
getDE = do
  h <- getDRegister
  l <- getERegister
  return $ makeWord16 l h

setDE :: (CPU m) => Word16 -> m ()
setDE nn = do
  setDRegister $ highByte nn
  setERegister $ lowByte nn

getHL :: (CPU m) => m Word16
getHL = do
  h <- getHRegister
  l <- getLRegister
  return $ makeWord16 l h

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
  getMemory (makeWord16 c 0xff)

setAtC :: (CPU m, Memory m) => Word8 -> m ()
setAtC v = do
  c <- getCRegister
  setMemory (makeWord16 c 0xff) v

getFlag :: CPU m => Flag -> m Bool
getFlag flag = do
  f <- getFRegister
  return $ testBit f (flagBit flag)

setFlags :: CPU m => [(Flag, Bool)] -> m ()
setFlags l = do
    f <- getFRegister
    setFRegister $ foldl setFlag f l
  where
    setFlag v (flag, True) = setBit v (flagBit flag)
    setFlag v (flag, False) = clearBit v (flagBit flag)

doAddA :: CPU m => Word8 -> m ()
doAddA n = do
  a <- getARegister
  let (res, h, c) = add8 a n
  setFlags [(ZFlag, res == 0), (NFlag, False), (HFlag, h), (CFlag, c)]
  setARegister res

doAddCA :: CPU m => Word8 -> m ()
doAddCA n = do
  a <- getARegister
  cflag <- getFlag CFlag
  let (res, h, c) = add8 a (n + if cflag then 1 else 0)
  setFlags [(ZFlag, res == 0), (NFlag, False), (HFlag, h), (CFlag, c)]
  setARegister res

doSubA :: CPU m => Word8 -> m ()
doSubA n = do
  a <- getARegister
  let (res, h, c) = sub8 a n
  setFlags [(ZFlag, res == 0), (NFlag, True), (HFlag, h), (CFlag, c)]
  setARegister res

doSubCA :: CPU m => Word8 -> m ()
doSubCA n = do
  a <- getARegister
  cflag <- getFlag CFlag
  let (res, h, c) = sub8 a (n + if cflag then 1 else 0)
  setFlags [(ZFlag, res == 0), (NFlag, True), (HFlag, h), (CFlag, c)]
  setARegister res

doAndA :: CPU m => Word8 -> m ()
doAndA n = do
  a <- getARegister
  let res = a .&. n
  setFlags [(ZFlag, res == 0), (NFlag, False), (HFlag, True), (CFlag, False)]
  setARegister res

doOrA :: CPU m => Word8 -> m ()
doOrA n = do
  a <- getARegister
  let res = a .|. n
  setFlags [(ZFlag, res == 0), (NFlag, False), (HFlag, False), (CFlag, False)]
  setARegister res

doXorA :: CPU m => Word8 -> m ()
doXorA n = do
  a <- getARegister
  let res = a `xor` n
  setFlags [(ZFlag, res == 0), (NFlag, False), (HFlag, False), (CFlag, False)]
  setARegister res

doCpA :: CPU m => Word8 -> m ()
doCpA n = do
  a <- getARegister
  let (res, h, c) = sub8 a n
  setFlags [(ZFlag, res == 0), (NFlag, True), (HFlag, h), (CFlag, c)]

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
  v <- getMemory (makeWord16 n 0xff)
  setARegister v
  tick 12

doInstruction (LDH_ATN_A n) = do
  v <- getARegister
  setMemory (makeWord16 n 0xff) v
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

doInstruction PUSH_AF = do
  getAF >>= pushStack16
  tick 16

doInstruction PUSH_BC = do
  getBC >>= pushStack16
  tick 16

doInstruction PUSH_DE = do
  getDE >>= pushStack16
  tick 16

doInstruction PUSH_HL = do
  getHL >>= pushStack16
  tick 16

doInstruction POP_AF = do
  popStack16 >>= setAF
  tick 12

doInstruction POP_BC = do
  popStack16 >>= setBC
  tick 12

doInstruction POP_DE = do
  popStack16 >>= setDE
  tick 12

doInstruction POP_HL = do
  popStack16 >>= setHL
  tick 12

doInstruction (ADD_A_R reg) = do
  getRegister reg >>= doAddA
  tick 4

doInstruction (ADD_A_N n) = do
  doAddA n
  tick 8

doInstruction ADD_A_ATHL = do
  getAtHL >>= doAddA
  tick 8

doInstruction (ADC_A_R reg) = do
  getRegister reg >>= doAddCA
  tick 4

doInstruction (ADC_A_N n) = do
  doAddCA n
  tick 8

doInstruction ADC_A_ATHL = do
  getAtHL >>= doAddCA
  tick 8

doInstruction (SUB_R reg) = do
  getRegister reg >>= doSubA
  tick 4

doInstruction (SUB_N n) = do
  doSubA n
  tick 8

doInstruction SUB_ATHL = do
  getAtHL >>= doSubA
  tick 8

doInstruction (SBC_A_R reg) = do
  getRegister reg >>= doSubCA
  tick 4

doInstruction (SBC_A_N n) = do
  doSubCA n
  tick 8

doInstruction SBC_A_ATHL = do
  getAtHL >>= doSubCA
  tick 8

doInstruction (AND_R reg) = do
  getRegister reg >>= doAndA
  tick 4

doInstruction (AND_N n) = do
  doAndA n
  tick 8

doInstruction AND_ATHL = do
  getAtHL >>= doAndA
  tick 8

doInstruction (OR_R reg) = do
  getRegister reg >>= doOrA
  tick 4

doInstruction (OR_N n) = do
  doOrA n
  tick 8

doInstruction OR_ATHL = do
  getAtHL >>= doOrA
  tick 8

doInstruction (XOR_R reg) = do
  getRegister reg >>= doXorA
  tick 4

doInstruction (XOR_N n) = do
  doXorA n
  tick 8

doInstruction XOR_ATHL = do
  getAtHL >>= doXorA
  tick 8

doInstruction (CP_R reg) = do
  getRegister reg >>= doCpA
  tick 4

doInstruction (CP_N n) = do
  doCpA n
  tick 8

doInstruction CP_ATHL = do
  getAtHL >>= doCpA
  tick 8

doInstruction (INC_R reg) = do
  v <- getRegister reg
  let (res, h, _) = add8 v 1
  setFlags [(ZFlag, res == 0), (NFlag, False), (HFlag, h)]
  setRegister reg res
  tick 4

doInstruction INC_ATHL = do
  v <- getAtHL
  let (res, h, _) = add8 v 1
  setFlags [(ZFlag, res == 0), (NFlag, False), (HFlag, h)]
  setAtHL res
  tick 12

doInstruction (DEC_R reg) = do
  v <- getRegister reg
  let (res, h, _) = sub8 v 1
  setFlags [(ZFlag, res == 0), (NFlag, True), (HFlag, h)]
  setRegister reg res
  tick 4

doInstruction DEC_ATHL = do
  v <- getAtHL
  let (res, h, _) = sub8 v 1
  setFlags [(ZFlag, res == 0), (NFlag, True), (HFlag, h)]
  setAtHL res
  tick 12

doInstruction NOP = tick 4
doInstruction STOP = stop >> tick 16

doInstruction _ = error "instruction step unimplemented!"
