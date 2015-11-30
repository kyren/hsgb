module Gameboy.CPU (
  CPU(..),
  Memory(..),
  step
) where

import Data.Word
import Data.Bits
import Control.Monad
import Gameboy.Util
import Gameboy.Instructions

class Monad m => CPU m where
  getARegister :: m Word8
  setARegister :: Word8 -> m ()

  getBRegister :: m Word8
  setBRegister :: Word8 -> m ()

  getCRegister :: m Word8
  setCRegister :: Word8 -> m ()

  getDRegister :: m Word8
  setDRegister :: Word8 -> m ()

  getERegister :: m Word8
  setERegister :: Word8 -> m ()

  getHRegister :: m Word8
  setHRegister :: Word8 -> m ()

  getLRegister :: m Word8
  setLRegister :: Word8 -> m ()

  getFRegister :: m Word8
  setFRegister :: Word8 -> m ()

  getProgramCounter :: m Word16
  setProgramCounter :: Word16 -> m ()

  getStackPointer :: m Word16
  setStackPointer :: Word16 -> m ()

  tick :: Int -> m ()

  halt :: m ()
  stop :: m ()

  disableInterrupts :: m ()
  enableInterrupts :: m ()

class Monad m => Memory m where
  getMemory :: Word16 -> m Word8
  setMemory :: Word16 -> Word8 -> m ()

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

bitNumber :: Bit -> Int
bitNumber Bit0 = 0
bitNumber Bit1 = 1
bitNumber Bit2 = 2
bitNumber Bit3 = 3
bitNumber Bit4 = 4
bitNumber Bit5 = 5
bitNumber Bit6 = 6
bitNumber Bit7 = 7

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
  setMemory16 (sp - 2) nn
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

testCond :: CPU m => Cond -> m Bool
testCond Zero = getFlag ZFlag
testCond NZero = not <$> getFlag ZFlag
testCond Carry = getFlag CFlag
testCond NCarry = not <$> getFlag CFlag

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

doAddHL :: CPU m => Word16 -> m ()
doAddHL nn = do
  hl <- getHL
  let (res, h, c) = add16 hl nn
  setFlags [(NFlag, False), (HFlag, h), (CFlag, c)]
  setHL res

doRLC :: CPU m => Register -> m ()
doRLC reg = do
  a <- getRegister reg
  let (r, c) = rotLC a
  setRegister reg r
  setFlags [(ZFlag, r == 0), (NFlag, False), (HFlag, False), (CFlag, c)]

doRL :: CPU m => Register -> m ()
doRL reg = do
  a <- getRegister reg
  c <- getFlag CFlag
  let (ar, cr) = rotL a c
  setRegister reg ar
  setFlags [(ZFlag, ar == 0), (NFlag, False), (HFlag, False), (CFlag, cr)]

doRRC :: CPU m => Register -> m ()
doRRC reg = do
  a <- getRegister reg
  let (r, c) = rotRC a
  setRegister reg r
  setFlags [(ZFlag, r == 0), (NFlag, False), (HFlag, False), (CFlag, c)]

doRR :: CPU m => Register -> m ()
doRR reg = do
  a <- getRegister reg
  c <- getFlag CFlag
  let (ar, cr) = rotR a c
  setRegister reg ar
  setFlags [(ZFlag, ar == 0), (NFlag, False), (HFlag, False), (CFlag, cr)]

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

doInstruction ADD_HL_BC = do
  getBC >>= doAddHL
  tick 8

doInstruction ADD_HL_DE = do
  getDE >>= doAddHL
  tick 8

doInstruction ADD_HL_HL = do
  getHL >>= doAddHL
  tick 8

doInstruction ADD_HL_SP = do
  getStackPointer >>= doAddHL
  tick 8

doInstruction (ADD_SP_N n) = do
  sp <- getStackPointer
  let (res, h, c) = add16 sp (fromIntegral n)
  setFlags [(ZFlag, False), (NFlag, False), (HFlag, h), (CFlag, c)]
  setStackPointer res
  tick 16

doInstruction INC_BC = do
  bc <- getBC
  setBC (bc + 1)
  tick 8

doInstruction INC_DE = do
  de <- getDE
  setDE (de + 1)
  tick 8

doInstruction INC_HL = do
  hl <- getHL
  setHL (hl + 1)
  tick 8

doInstruction INC_SP = do
  sp <- getHL
  setHL (sp + 1)
  tick 8

doInstruction DEC_BC = do
  bc <- getBC
  setBC (bc - 1)
  tick 8

doInstruction DEC_DE = do
  de <- getDE
  setDE (de - 1)
  tick 8

doInstruction DEC_HL = do
  hl <- getHL
  setHL (hl - 1)
  tick 8

doInstruction DEC_SP = do
  sp <- getHL
  setHL (sp - 1)
  tick 8

doInstruction (SWAP_R r) = do
  n <- getRegister r
  let ln = lowNibble n
  let hn = highNibble n
  setRegister r (makeWord8 hn ln)
  tick 8

doInstruction SWAP_ATHL = do
  r <- getAtHL
  let ln = lowNibble r
  let hn = highNibble r
  setAtHL (makeWord8 hn ln)
  tick 16

doInstruction DAA = error "DAA is hard"

doInstruction CPL = do
  a <- getARegister
  setARegister (complement a)
  setFlags [(NFlag, True), (HFlag, True)]
  tick 4

doInstruction CCF = do
  c <- getFlag CFlag
  setFlags [(NFlag, False), (HFlag, False), (CFlag, not c)]
  tick 4

doInstruction SCF = do
  setFlags [(NFlag, False), (HFlag, False), (CFlag, True)]
  tick 4

doInstruction NOP = tick 4
doInstruction HALT = halt >> tick 4
doInstruction STOP = stop >> tick 4
doInstruction DI = disableInterrupts
doInstruction EI = enableInterrupts

doInstruction RLCA = do
  doRLC ARegister
  tick 4

doInstruction RLA = do
  doRL ARegister
  tick 4

doInstruction RRCA = do
  doRRC ARegister
  tick 4

doInstruction RRA = do
  doRR ARegister
  tick 4

doInstruction (RLC_R r) = do
  doRLC r
  tick 8

doInstruction RLC_ATHL = do
  a <- getAtHL
  let (r, c) = rotLC a
  setAtHL r
  setFlags [(ZFlag, r == 0), (NFlag, False), (HFlag, False), (CFlag, c)]
  tick 16

doInstruction (RL_R r) = do
  doRL r
  tick 8

doInstruction RL_ATHL = do
  a <- getAtHL
  c <- getFlag CFlag
  let (ar, cr) = rotL a c
  setAtHL ar
  setFlags [(ZFlag, ar == 0), (NFlag, False), (HFlag, False), (CFlag, cr)]
  tick 16

doInstruction (RRC_R r) = do
  doRRC r
  tick 8

doInstruction RRC_ATHL = do
  a <- getAtHL
  let (r, c) = rotRC a
  setAtHL r
  setFlags [(ZFlag, r == 0), (NFlag, False), (HFlag, False), (CFlag, c)]
  tick 16

doInstruction (RR_R r) = do
  doRR r
  tick 8

doInstruction RR_ATHL = do
  a <- getAtHL
  c <- getFlag CFlag
  let (ar, cr) = rotR a c
  setAtHL ar
  setFlags [(ZFlag, ar == 0), (NFlag, False), (HFlag, False), (CFlag, cr)]
  tick 16

doInstruction (SLA_R r) = do
  b <- getRegister r
  let c = testBit b 7
  let bs = shift b 1
  setRegister r bs
  setFlags [(ZFlag, bs == 0), (NFlag, False), (HFlag, False), (CFlag, c)]
  tick 8

doInstruction SLA_ATHL = do
  b <- getAtHL
  let c = testBit b 7
  let bs = shift b 1
  setAtHL bs
  setFlags [(ZFlag, bs == 0), (NFlag, False), (HFlag, False), (CFlag, c)]
  tick 16

doInstruction (SRA_R r) = do
  b <- getRegister r
  let lsb = testBit b 0
  let msb = testBit b 7
  let bshift = shift b (-1)
  let res = if msb then setBit bshift 7 else clearBit bshift 7
  setRegister r res
  setFlags [(ZFlag, res == 0), (NFlag, False), (HFlag, False), (CFlag, lsb)]
  tick 8

doInstruction SRA_ATHL = do
  b <- getAtHL
  let lsb = testBit b 0
  let msb = testBit b 7
  let bshift = shift b (-1)
  let res = if msb then setBit bshift 7 else clearBit bshift 7
  setAtHL res
  setFlags [(ZFlag, res == 0), (NFlag, False), (HFlag, False), (CFlag, lsb)]
  tick 16

doInstruction (SRL_R r) = do
  b <- getRegister r
  let lsb = testBit b 0
  let res = shift b (-1)
  setRegister r res
  setFlags [(ZFlag, res == 0), (NFlag, False), (HFlag, False), (CFlag, lsb)]
  tick 8

doInstruction SRL_ATHL = do
  b <- getAtHL
  let lsb = testBit b 0
  let res = shift b (-1)
  setAtHL res
  setFlags [(ZFlag, res == 0), (NFlag, False), (HFlag, False), (CFlag, lsb)]
  tick 16

doInstruction (BIT_B_R b r) = do
  val <- getRegister r
  let btest = testBit val (bitNumber b)
  setFlags [(ZFlag, not btest), (NFlag, False), (HFlag, True)]
  tick 8

doInstruction (BIT_B_ATHL b) = do
  val <- getAtHL
  let btest = testBit val (bitNumber b)
  setFlags [(ZFlag, not btest), (NFlag, False), (HFlag, True)]
  tick 16

doInstruction (SET_B_R b r) = do
  val <- getRegister r
  setRegister r (setBit val (bitNumber b))
  tick 8

doInstruction (SET_B_ATHL b) = do
  val <- getAtHL
  setAtHL (setBit val (bitNumber b))
  tick 16

doInstruction (RES_B_R b r) = do
  val <- getRegister r
  setRegister r (clearBit val (bitNumber b))
  tick 8

doInstruction (RES_B_ATHL b) = do
  val <- getAtHL
  setAtHL (clearBit val (bitNumber b))
  tick 16

doInstruction (JP_NN nn) = do
  setProgramCounter nn
  tick 12

doInstruction (JP_C_NN cond nn) = do
  c <- testCond cond
  unless c (setProgramCounter nn)
  tick 12

doInstruction (JP_ATHL) = do
  addr <- getHL
  setProgramCounter addr
  tick 4

doInstruction (JR_N n) = do
  pc <- getProgramCounter
  setProgramCounter (pc + fromIntegral n)
  tick 8

doInstruction (JR_C_N cond n) = do
  c <- testCond cond
  when c $ do
    pc <- getProgramCounter
    setProgramCounter (pc + fromIntegral n)
  tick 8

doInstruction (CALL_NN nn) = do
  pc <- getProgramCounter
  pushStack16 pc
  setProgramCounter nn
  tick 12

doInstruction (CALL_C_NN cond nn) = do
  c <- testCond cond
  when c $ do
    pc <- getProgramCounter
    pushStack16 pc
    setProgramCounter nn
  tick 12

doInstruction (RST_RA ra) = do
    getProgramCounter >>= pushStack16
    setProgramCounter (resetAddress ra)
    tick 32
  where
    resetAddress Reset00 = 0x00
    resetAddress Reset08 = 0x08
    resetAddress Reset10 = 0x10
    resetAddress Reset18 = 0x18
    resetAddress Reset20 = 0x20
    resetAddress Reset28 = 0x28
    resetAddress Reset30 = 0x30
    resetAddress Reset38 = 0x38

doInstruction (RET) = do
  nn <- popStack16
  setProgramCounter nn
  tick 8

doInstruction (RET_C cond) = do
  c <- testCond cond
  when c $ do
    nn <- popStack16
    setProgramCounter nn
  tick 8

doInstruction (RETI) = do
  nn <- popStack16
  setProgramCounter nn
  enableInterrupts
  tick 8
