module Gameboy.CPU where

import Data.Word
import Data.Bits

class Monad m => Memory m where
  peek :: Word16 -> m Word8
  poke :: Word16 -> Word8 -> m ()

data Register = ARegister | BRegister | CRegister | DRegister | ERegister | HRegister | LRegister | FRegister

class Monad m => CPU m where
  register :: Register -> m Word8
  setRegister :: Register -> Word8 -> m ()
  programCounter :: m Word16
  setProgramCounter :: Word16 -> m ()
  stackPointer :: m Word16
  setStackPointer :: Word16 -> m ()
  tick :: Int -> m ()

makeWord :: Word8 -> Word8 -> Word16
makeWord h l = shift (fromIntegral h) 8 .|. fromIntegral l

highByte :: Word16 -> Word8
highByte w = fromIntegral (shift w (-8))

lowByte :: Word16 -> Word8
lowByte = fromIntegral

{-
data CPU = CPU {
    _aRegister, _bRegister, _cRegister, _dRegister, _eRegister, _hRegister, _lRegister, _fRegister:: Word8,
    _programCounter :: Word16,
    _stackPointer :: Word16,
    _mClock :: Integer,
    _tClock :: Integer
  }
makeLenses ''CPU

type Register = Lens' CPU Word8

initCPU :: CPU
initCPU = CPU 0 0 0 0 0 0 0 0 0 0 0 0

class Monad m => Memory m where
  peek :: Word16 -> m Word8
  poke :: Word16 -> Word8 -> m ()

reset :: MonadState CPU m => m ()
reset = put initCPU

tick :: MonadState CPU m => Integer -> m ()
tick mCycles = do
  mClock += mCycles
  tClock += mCycles * 4

getReg8 :: MonadState CPU m => Register -> m Word8
getReg8 reg = use reg

setReg8 :: MonadState CPU m => Register -> Word8 -> m ()
setReg8 reg = assign reg

getReg16 :: MonadState CPU m => Register -> Register -> m Word16
getReg16 regh regl = do
  high <- use regh
  low <- use regl
  return $ makeWord high low

setReg16 :: MonadState CPU m => Register -> Register -> Word16 -> m ()
setReg16 regh regl val = do
  regh .= highByte val
  regl .= lowByte val

getMem8 :: Memory m => Word16 -> m Word8
getMem8 = peek

setMem8 :: Memory m => Word16 -> Word8 -> m ()
setMem8 = poke

getMem16 :: Memory m => Word16 -> m Word16
getMem16 addr = do
  high <- peek addr
  low <- peek (addr + 1)
  return $ makeWord high low

setMem16 :: Memory m => Word16 -> Word16 -> m ()
setMem16 addr val = do
  poke addr (highByte val)
  poke (addr + 1) (lowByte val)

getPC8 :: (MonadState CPU m, Memory m) => m Word8
getPC8 = do
  pc <- use programCounter
  programCounter += 1
  getMem8 pc

getPC16 :: (MonadState CPU m, Memory m) => m Word16
getPC16 = do
  pc <- use programCounter
  programCounter += 2
  getMem16 pc

pushStack8 :: (MonadState CPU m, Memory m) => Word8 -> m ()
pushStack8 val = do
  stackPointer -= 1
  sp <- use stackPointer
  setMem8 sp val

pushStack16 :: (MonadState CPU m, Memory m) => Word16 -> m ()
pushStack16 val = do
  pushStack8 (highByte val)
  pushStack8 (lowByte val)

popStack8 :: (MonadState CPU m, Memory m) => m Word8
popStack8 = do
  sp <- use stackPointer
  val <- getMem8 sp
  stackPointer -= 1
  return val

popStack16 :: (MonadState CPU m, Memory m) => m Word16
popStack16 = do
  l <- popStack8 
  h <- popStack8 
  return $ makeWord h l

load_rx_rx :: MonadState CPU m => Register -> Register -> m ()
load_rx_rx sourceReg targetReg = do
  getReg8 sourceReg >>= setReg8 targetReg
  tick 1

load_rx_mhl :: (MonadState CPU m, Memory m) => Register -> m ()
load_rx_mhl targetReg = do
  getReg16 hRegister lRegister >>= getMem8 >>= setReg8 targetReg
  tick 2

load_mhl_rx :: (MonadState CPU m, Memory m) => Register -> m ()
load_mhl_rx sourceReg = do
  addr <- getReg16 hRegister lRegister
  val <- getReg8 sourceReg
  setMem8 addr val
  tick 2

load_rx_n :: (MonadState CPU m, Memory m) => Register -> m ()
load_rx_n targetReg = do
  getPC8 >>= setReg8 targetReg
  tick 2

load_mhl_n :: (MonadState CPU m, Memory m) => m ()
load_mhl_n = do
  v <- getPC8
  addr <- getReg16 hRegister lRegister
  setMem8 addr v
  tick 2

load_mxx_rx :: (MonadState CPU m, Memory m) => Register -> Register -> Register -> m ()
load_mxx_rx hReg lReg sourceReg = do
  addr <- getReg16 hReg lReg
  v <- getReg8 sourceReg
  setMem8 addr v
  tick 2

load_mnn_rx :: (MonadState CPU m, Memory m) => Register -> m ()
load_mnn_rx sourceReg = do
  addr <- getPC16
  val <- getReg8 sourceReg
  setMem8 addr val
  tick 4

load_rx_mxx :: (MonadState CPU m, Memory m) => Register -> Register -> Register -> m ()
load_rx_mxx targetReg memReg1 memReg2 = do
  addr <- getReg16 memReg1 memReg2
  val <- getMem8 addr
  setReg8 targetReg val
  tick 2

load_rx_mnn :: (MonadState CPU m, Memory m) => Register -> m ()
load_rx_mnn targetReg = do
  addr <- getPC16
  val <- getMem8 addr
  setReg8 targetReg val
  tick 4

pushr :: (MonadState CPU m, Memory m) => Register -> Register -> m ()
pushr reg1 reg2 = do
  val <- getReg16 reg1 reg2
  pushStack16 val
  tick 3

popr :: (MonadState CPU m, Memory m) => Register -> Register -> m ()
popr reg1 reg2 = do
  val <- popStack16
  setReg8 reg1 (highByte val)
  setReg8 reg2 (lowByte val)
  tick 3

-}
