{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Gameboy.CPU (
  CPU,
  aRegister, bRegister, cRegister, dRegister, eRegister, hRegister, lRegister,
  flags, programCounter, stackPointer, mClock, tClock,
  initCPU,
  Memory,
  peekWord,
  pokeWord,
  reset,
  pushBC,
  popHL,
  loadA
) where

import Data.Word
import Data.Bits
import Control.Monad.State
import Control.Lens

data CPU = CPU {
    _aRegister, _bRegister, _cRegister, _dRegister, _eRegister, _hRegister, _lRegister :: Word8,
    _flags :: Word8,
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

peekWord :: Memory m => Word16 -> m Word16
peekWord addr = do
  low <- peek addr
  high <- peek (addr + 1)
  return (shift (fromIntegral high) 8 .|. fromIntegral low)

pokeWord :: Memory m => Word16 -> Word16 -> m ()
pokeWord addr val = do
  let low = fromIntegral val
  let high = fromIntegral (shift val (-8))
  poke addr low
  poke (addr + 1) high

reset :: MonadState CPU m => m ()
reset = put initCPU

pushBC :: (MonadState CPU m, Memory m) => m ()
pushBC = do
  cpu <- get
  poke (cpu^.stackPointer - 1) (cpu^.bRegister)
  poke (cpu^.stackPointer - 2) (cpu^.bRegister)
  stackPointer -= 2
  mClock += 3
  tClock += 12

popHL :: (MonadState CPU m, Memory m) => m ()
popHL = do
  cpu <- get
  hval <- peek (cpu^.stackPointer + 1)
  lval <- peek (cpu^.stackPointer)
  hRegister .= hval
  lRegister .= lval
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
