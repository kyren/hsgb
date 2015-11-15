{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Gameboy.Emulation (
  Pixel(..),
  horizontalScreenPixels,
  verticalScreenPixels,
  Screen,
  pixelAt,
  EmulatorState,
  initialState,
  renderFrame,
  stepFrame
) where

import Data.Word
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.STRef
import Control.Monad.ST
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Gameboy.CPU

data Pixel = Black | DarkGray | LightGray | White

horizontalScreenPixels :: Int
horizontalScreenPixels = 160

verticalScreenPixels :: Int
verticalScreenPixels = 144

type Screen = V.Vector Pixel

pixelAt :: V.Vector Pixel -> Int -> Int -> Pixel
pixelAt screen x y = screen V.! (y * horizontalScreenPixels + x)

data EmulatorState = EmulatorState {
    interruptsEnabled :: Word8,
    stackPointer :: Word16,
    programCounter :: Word16,

    aRegister, bRegister, cRegister, dRegister, eRegister, hRegister, lRegister, fRegister :: Word8,

    cartridgeRomBank0 :: VU.Vector Word8,
    cartridgeRomBank1 :: VU.Vector Word8,

    internalRamBank0 :: VU.Vector Word8,
    zeroPage :: VU.Vector Word8,

    characterRam :: VU.Vector Word8,
    bgMapData :: VU.Vector Word8,
    spriteAttributeData :: VU.Vector Word8
  }

initialState :: EmulatorState
initialState = EmulatorState {
      interruptsEnabled = 0x0f,
      stackPointer = 0,
      programCounter = 0x100,
      aRegister = 0,
      bRegister = 0,
      cRegister = 0,
      dRegister = 0,
      eRegister = 0,
      hRegister = 0,
      lRegister = 0,
      fRegister = 0,
      cartridgeRomBank0 = VU.replicate 0x4000 0x0,
      cartridgeRomBank1 = VU.replicate 0x4000 0x0,
      internalRamBank0 = VU.replicate 0x1000 0x0,
      zeroPage = VU.replicate 0x79 0x0,
      bgMapData = VU.replicate 0x800 0x0,
      characterRam = VU.replicate 0x1800 0x0,
      spriteAttributeData = VU.replicate 0x80 0x0
    }

renderFrame :: EmulatorState -> Screen
renderFrame _ = V.fromList $ take (horizontalScreenPixels * verticalScreenPixels) $ cycle [Black, Black, DarkGray, DarkGray, LightGray, LightGray, White, White, White]

stepFrame :: EmulatorState -> EmulatorState
stepFrame state = state

data WorkingState s = WorkingState {
    workingInterruptsEnabled :: STRef s Word8,
    workingStackPointer :: STRef s Word16,
    workingProgramCounter :: STRef s Word16,

    workingARegister, workingBRegister, workingCRegister, workingDRegister, workingERegister, workingHRegister, workingLRegister, workingFRegister :: STRef s Word8,

    workingCartridgeRomBank0 :: VUM.MVector s Word8,
    workingCartridgeRomBank1 :: VUM.MVector s Word8,

    workingInternalRamBank0 :: VUM.MVector s Word8,
    workingZeroPage :: VUM.MVector s Word8,

    workingCharacterRam :: VUM.MVector s Word8,
    workingBgMapData :: VUM.MVector s Word8,
    workingSpriteAttributeData :: VUM.MVector s Word8
  }

newtype WorkingEnvironment s a = WorkingEnvironment {
    runWorkingEnvironment :: ReaderT (WorkingState s) (ST s) a
  } deriving (Monad, Applicative, Functor)

readRef :: (WorkingState s -> STRef s v) -> WorkingEnvironment s v
readRef field = WorkingEnvironment $ do
  state <- ask
  lift $ readSTRef (field state)

writeRef :: (WorkingState s -> STRef s v) -> v -> WorkingEnvironment s ()
writeRef field v = WorkingEnvironment $ do
  state <- ask
  lift $ writeSTRef (field state) v

readMem :: (WorkingState s -> VUM.MVector s Word8) -> Word16 -> WorkingEnvironment s Word8
readMem bank addr = WorkingEnvironment $ do
  state <- ask
  lift $ VUM.read (bank state) (fromIntegral addr)

writeMem :: (WorkingState s -> VUM.MVector s Word8) -> Word16 -> Word8 -> WorkingEnvironment s ()
writeMem bank addr byte = WorkingEnvironment $ do
  state <- ask
  lift $ VUM.write (bank state) (fromIntegral addr) byte

instance CPU (WorkingEnvironment s) where
  getARegister = readRef workingARegister
  setARegister = writeRef workingARegister

  getBRegister = readRef workingBRegister
  setBRegister = writeRef workingBRegister

  getCRegister = readRef workingCRegister
  setCRegister = writeRef workingCRegister

  getDRegister = readRef workingDRegister
  setDRegister = writeRef workingDRegister

  getERegister = readRef workingERegister
  setERegister = writeRef workingERegister

  getHRegister = readRef workingHRegister
  setHRegister = writeRef workingHRegister

  getLRegister = readRef workingLRegister
  setLRegister = writeRef workingLRegister

  getFRegister = readRef workingFRegister
  setFRegister = writeRef workingFRegister

  getProgramCounter = readRef workingProgramCounter
  setProgramCounter = writeRef workingProgramCounter

  getStackPointer = readRef workingStackPointer
  setStackPointer = writeRef workingStackPointer

  halt = return ()
  stop = return ()
  disableInterrupts = return ()
  enableInterrupts = return ()
  tick _ = return ()

instance Memory (WorkingEnvironment s) where
  getMemory addr
    | addr < 0x4000 = readMem workingCartridgeRomBank0 addr
    | addr < 0x8000 = readMem workingCartridgeRomBank1 (addr - 0x4000)
    | addr < 0x9800 = readMem workingCharacterRam (addr - 0x8000)
    | addr < 0xa000 = readMem workingBgMapData (addr - 0x9800)
    | addr < 0xc000 = fail "Illegal read of cartridge ram bank"
    | addr < 0xd000 = readMem workingInternalRamBank0 (addr - 0xc000)
    | addr < 0xfe00 = fail "Unimplemented memory region"
    | addr < 0xfea0 = readMem workingSpriteAttributeData (addr - 0xfe00)
    | addr < 0xff80 = fail "Unimplemented memory region"
    | addr < 0xffff = readMem workingZeroPage (addr - 0xfe80)
    | otherwise = readRef workingInterruptsEnabled

  setMemory addr byte
    | addr < 0x4000 = fail "Illegal write of cartridge rom"
    | addr < 0x8000 = fail "Illegal write of cartridge rom"
    | addr < 0x9800 = writeMem workingCharacterRam (addr - 0x8000) byte
    | addr < 0xa000 = writeMem workingBgMapData (addr - 0x9800) byte
    | addr < 0xc000 = fail "Illegal write of cartridge ram bank"
    | addr < 0xd000 = writeMem workingInternalRamBank0 (addr - 0xc000) byte
    | addr < 0xfe00 = fail "Unimplemented memory region"
    | addr < 0xfea0 = writeMem workingSpriteAttributeData (addr - 0xfe00) byte
    | addr < 0xff80 = fail "Unimplemented memory region"
    | addr < 0xffff = writeMem workingZeroPage (addr - 0xfe80) byte
    | otherwise = writeRef workingInterruptsEnabled byte
