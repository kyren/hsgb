module Gameboy.CPU (
  Memory(..),
  CPU(..)
) where

import Data.Word

class Monad m => Memory m where
  getMemory :: Word16 -> m Word8
  setMemory :: Word16 -> Word8 -> m ()

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
