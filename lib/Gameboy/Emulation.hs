{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Gameboy.Emulation (
  EmulatorState(..),
  initialState,
  loadRom,
  stepEmulator
) where

import Data.Word
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.ByteString as BS
import Data.STRef
import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Gameboy.CPU

data EmulatorState = EmulatorState {
    interruptsEnabled :: Word8,
    stackPointer :: Word16,
    programCounter :: Word16,

    aRegister, bRegister, cRegister, dRegister, eRegister, hRegister, lRegister, fRegister :: Word8,

    cartridgeRomBank0 :: VU.Vector Word8,
    cartridgeRomBank1 :: VU.Vector Word8,

    internalRamBank0 :: VU.Vector Word8,
    internalRamBank1 :: VU.Vector Word8,
    zeroPage :: VU.Vector Word8,

    characterRam :: VU.Vector Word8,
    bgMapData :: VU.Vector Word8,
    spriteAttributeData :: VU.Vector Word8
  } deriving Show

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
    internalRamBank1 = VU.replicate 0x1000 0x0,
    zeroPage = VU.replicate 0x79 0x0,
    bgMapData = VU.replicate 0x800 0x0,
    characterRam = VU.replicate 0x1800 0x0,
    spriteAttributeData = VU.replicate 0x80 0x0
  }

loadRom :: BS.ByteString -> Either String EmulatorState
loadRom rom = if BS.length rom == 0x8000 then Right state else Left "Improper rom size"
  where
    state = initialState { cartridgeRomBank0 = bank0, cartridgeRomBank1 = bank1 }
    bank0 = VU.generate 0x4000 (BS.index rom)
    bank1 = VU.generate 0x4000 (\i -> BS.index rom (i + 0x4000))

data WorkingState s = WorkingState {
    workingInterruptsEnabled :: STRef s Word8,
    workingStackPointer :: STRef s Word16,
    workingProgramCounter :: STRef s Word16,

    workingARegister, workingBRegister, workingCRegister, workingDRegister, workingERegister, workingHRegister, workingLRegister, workingFRegister :: STRef s Word8,

    workingCartridgeRomBank0 :: VUM.MVector s Word8,
    workingCartridgeRomBank1 :: VUM.MVector s Word8,

    workingInternalRamBank0 :: VUM.MVector s Word8,
    workingInternalRamBank1 :: VUM.MVector s Word8,
    workingZeroPage :: VUM.MVector s Word8,

    workingCharacterRam :: VUM.MVector s Word8,
    workingBgMapData :: VUM.MVector s Word8,
    workingSpriteAttributeData :: VUM.MVector s Word8
  }

newtype WorkingEnvironment s a = WorkingEnvironment {
    runWorkingEnvironment :: ReaderT (WorkingState s) (ST s) a
  } deriving (Monad, Applicative, Functor)

freezeWorkingState :: WorkingState s -> ST s EmulatorState
freezeWorkingState st = EmulatorState <$>
    getRef workingInterruptsEnabled <*>
    getRef workingStackPointer <*>
    getRef workingProgramCounter <*>
    getRef workingARegister <*>
    getRef workingBRegister <*>
    getRef workingCRegister <*>
    getRef workingDRegister <*>
    getRef workingERegister <*>
    getRef workingHRegister <*>
    getRef workingLRegister <*>
    getRef workingFRegister <*>
    freezeVector workingCartridgeRomBank0 <*>
    freezeVector workingCartridgeRomBank1 <*>
    freezeVector workingInternalRamBank0 <*>
    freezeVector workingInternalRamBank1 <*>
    freezeVector workingZeroPage <*>
    freezeVector workingCharacterRam <*>
    freezeVector workingBgMapData <*>
    freezeVector workingSpriteAttributeData
  where
    getRef f = readSTRef (f st)
    freezeVector f = VU.freeze (f st)

thawWorkingState :: EmulatorState -> ST s (WorkingState s)
thawWorkingState st = WorkingState <$>
    makeRef interruptsEnabled <*>
    makeRef stackPointer <*>
    makeRef programCounter <*>
    makeRef aRegister <*>
    makeRef bRegister <*>
    makeRef cRegister <*>
    makeRef dRegister <*>
    makeRef eRegister <*>
    makeRef hRegister <*>
    makeRef lRegister <*>
    makeRef fRegister <*>
    thawVector cartridgeRomBank0 <*>
    thawVector cartridgeRomBank1 <*>
    thawVector internalRamBank0 <*>
    thawVector internalRamBank1 <*>
    thawVector zeroPage <*>
    thawVector characterRam <*>
    thawVector bgMapData <*>
    thawVector spriteAttributeData
  where
    makeRef f = newSTRef (f st)
    thawVector f = VU.thaw (f st)

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
    | addr < 0xc000 = fail $ "Illegal read from cartridge ram bank " ++ show addr
    | addr < 0xd000 = readMem workingInternalRamBank0 (addr - 0xc000)
    | addr < 0xe000 = readMem workingInternalRamBank1 (addr - 0xd000)
    | addr < 0xfe00 = getMemory (addr - 0x2000)
    | addr < 0xfea0 = readMem workingSpriteAttributeData (addr - 0xfe00)
    | addr < 0xff00 = fail $ "Illegal read from unusable memory region " ++ show addr
    | addr < 0xff80 = return 0x0 -- TODO: Implement hardware registers
    | addr < 0xffff = readMem workingZeroPage (addr - 0xff80)
    | otherwise = readRef workingInterruptsEnabled

  setMemory addr byte
    | addr < 0x4000 = fail $ "Illegal write to cartridge rom " ++ show addr
    | addr < 0x8000 = fail $ "Illegal write to cartridge rom " ++ show addr
    | addr < 0x9800 = writeMem workingCharacterRam (addr - 0x8000) byte
    | addr < 0xa000 = writeMem workingBgMapData (addr - 0x9800) byte
    | addr < 0xc000 = fail $ "Illegal write to cartridge ram bank " ++ show addr
    | addr < 0xd000 = writeMem workingInternalRamBank0 (addr - 0xc000) byte
    | addr < 0xe000 = writeMem workingInternalRamBank1 (addr - 0xd000) byte
    | addr < 0xfe00 = setMemory (addr - 0x2000) byte
    | addr < 0xfea0 = writeMem workingSpriteAttributeData (addr - 0xfe00) byte
    | addr < 0xff00 = fail $ "Illegal write to unusable memory region " ++ show addr
    | addr < 0xff80 = return () -- TODO: Implement hardware registers
    | addr < 0xffff = writeMem workingZeroPage (addr - 0xff80) byte
    | otherwise = writeRef workingInterruptsEnabled byte

stepEmulator :: EmulatorState -> Int -> EmulatorState
stepEmulator state cycles = runST $ do
    working <- thawWorkingState state
    (runReaderT . runWorkingEnvironment) (replicateM_ cycles step) working
    freezeWorkingState working
