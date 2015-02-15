{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Gameboy.CPU (
  CPU,
  aRegister, bRegister, cRegister, dRegister, eRegister, hRegister, lRegister, fRegister,
  programCounter, stackPointer, mClock, tClock,
  initCPU,
  Memory,
  peekWord,
  pokeWord,
  reset,
  loadrr,
  loadrm,
  loadmr,
  pushRegisters,
  popRegisters,
  loadA
) where

import Data.Word
import Data.Bits
import Control.Monad.State
import Control.Lens

data CPU = CPU {
    _aRegister, _bRegister, _cRegister, _dRegister, _eRegister, _hRegister, _lRegister, _fRegister:: Word8,
    _programCounter :: Word16,
    _stackPointer :: Word16,
    _mClock :: Integer,
    _tClock :: Integer
  }
makeLenses ''CPU

initCPU :: CPU
initCPU = CPU 0 0 0 0 0 0 0 0 0 0 0 0

class Monad m => Memory m where
  peek :: Word16 -> m Word8
  poke :: Word16 -> Word8 -> m ()

makeWord :: (Word8, Word8) -> Word16
makeWord (h, l) = shift (fromIntegral h) 8 .|. fromIntegral l

separateWord :: Word16 -> (Word8, Word8)
separateWord w = (fromIntegral (shift w (-8)), fromIntegral w)

peekWord :: Memory m => Word16 -> m Word16
peekWord addr = do
  high <- peek (addr + 1)
  low <- peek addr
  return $ makeWord (high, low)

pokeWord :: Memory m => Word16 -> Word16 -> m ()
pokeWord addr w = do
  let (high, low) = separateWord w
  poke (addr + 1) high
  poke addr low

reset :: MonadState CPU m => m ()
reset = put initCPU

loadrr :: MonadState CPU m => Getter CPU Word8 -> Setter' CPU Word8 -> m ()
loadrr source target = do
  val <- use source
  target .= val
  mClock += 1
  tClock += 4

loadrm :: (MonadState CPU m, Memory m) => Setter' CPU Word8 -> m ()
loadrm target = do
  cpu <- get
  v <- peek $ makeWord (cpu^.hRegister, cpu^.lRegister)
  target .= v
  mClock += 2
  tClock += 8

loadmr :: (MonadState CPU m, Memory m) => Getter CPU Word8 -> m ()
loadmr source = do
  cpu <- get
  poke (makeWord (cpu^.hRegister, cpu^.lRegister)) (cpu^.source)
  mClock += 2
  tClock += 8

pushRegisters :: (MonadState CPU m, Memory m) => Getter CPU Word8 -> Getter CPU Word8 -> m ()
pushRegisters reg1 reg2 = do
  cpu <- get
  poke (cpu^.stackPointer - 1) (cpu^.reg1)
  poke (cpu^.stackPointer - 2) (cpu^.reg2)
  stackPointer -= 2
  mClock += 3
  tClock += 12

popRegisters :: (MonadState CPU m, Memory m) => Setter' CPU Word8 -> Setter' CPU Word8 -> m ()
popRegisters reg1 reg2 = do
  cpu <- get
  hval <- peek (cpu^.stackPointer + 1)
  lval <- peek (cpu^.stackPointer)
  reg1 .= hval
  reg2 .= lval
  stackPointer += 2
  mClock += 3
  tClock += 12

loadA :: (MonadState CPU m, Memory m) => m ()
loadA = do
  cpu <- get
  addr <- peekWord (cpu^.stackPointer)
  val <- peek addr
  aRegister .= val
  programCounter += 2
  mClock += 4
  tClock += 16
