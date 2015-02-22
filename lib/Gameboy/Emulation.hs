{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Gameboy.Emulation where

import Data.Word
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.STRef
import Gameboy.CPU
import Gameboy.Instructions

data SimpleState = SimpleState {
    memory :: VU.Vector Word8,
    aRegister, bRegister, cRegister, dRegister, eRegister, hRegister, lRegister, fRegister :: Word8,
    programCounter :: Word16,
    stackPointer :: Word16,
    clock :: Word64
  }
  deriving Show

data SimpleStateST s = SimpleStateST {
    stMemory :: VUM.MVector s Word8,
    stARegister, stBRegister, stCRegister, stDRegister, stERegister, stHRegister, stLRegister, stFRegister :: STRef s Word8,
    stProgramCounter :: STRef s Word16,
    stStackPointer :: STRef s Word16,
    stClock :: STRef s Word64
  }

newtype SimpleEnvironmentST s a = SimpleEnvironmentST (ReaderT (SimpleStateST s) (ST s) a)
  deriving (Monad, Applicative, Functor)

initialState :: SimpleState
initialState = SimpleState (VU.replicate 0x10000 0) 0 0 0 0 0 0 0 0 0 0 0

freezeState :: SimpleStateST s -> ST s SimpleState
freezeState st = SimpleState <$>
    VU.freeze (stMemory st) <*> 
    rd stARegister <*>
    rd stBRegister <*>
    rd stCRegister <*>
    rd stDRegister <*>
    rd stERegister <*>
    rd stHRegister <*>
    rd stLRegister <*>
    rd stFRegister <*>
    rd stProgramCounter <*>
    rd stStackPointer <*>
    rd stClock
  where
    rd f = readSTRef (f st)

thawState :: SimpleState -> ST s (SimpleStateST s)
thawState st = SimpleStateST <$>
    VU.thaw (memory st) <*>
    mk aRegister <*>
    mk bRegister <*>
    mk cRegister <*>
    mk dRegister <*>
    mk eRegister <*>
    mk hRegister <*>
    mk lRegister <*>
    mk fRegister <*>
    mk programCounter <*>
    mk stackPointer <*>
    mk clock
  where
    mk f = newSTRef (f st)

instance Memory (SimpleEnvironmentST s) where
  getMemory addr = SimpleEnvironmentST $ do
    state <- ask
    lift $ VUM.read (stMemory state) (fromIntegral addr)
  setMemory addr byte = SimpleEnvironmentST $ do
    state <- ask
    lift $ VUM.write (stMemory state) (fromIntegral addr) byte

registerRef :: Register -> SimpleStateST s -> STRef s Word8
registerRef ARegister = stARegister
registerRef BRegister = stBRegister
registerRef CRegister = stCRegister
registerRef DRegister = stDRegister
registerRef ERegister = stERegister
registerRef HRegister = stHRegister
registerRef LRegister = stLRegister
registerRef FRegister = stFRegister

readRef :: (SimpleStateST s -> STRef s v) -> SimpleEnvironmentST s v
readRef field = SimpleEnvironmentST $ do
  state <- ask
  lift $ readSTRef (field state)

writeRef :: (SimpleStateST s -> STRef s v) -> v -> SimpleEnvironmentST s ()
writeRef field v = SimpleEnvironmentST $ do
  state <- ask
  lift $ writeSTRef (field state) v

instance CPU (SimpleEnvironmentST s) where
  getRegister r = readRef (registerRef r)
  setRegister r = writeRef (registerRef r)

  getProgramCounter = readRef stProgramCounter
  setProgramCounter = writeRef stProgramCounter

  getStackPointer = readRef stStackPointer
  setStackPointer = writeRef stStackPointer

  tick n = do
    c <- readRef stClock
    writeRef stClock (c + fromIntegral n)

runSimpleState :: SimpleState -> Int -> SimpleState
runSimpleState initial count =
  runST $ go (replicateM_ count step)
  where
    go (SimpleEnvironmentST steps) = do
      st <- thawState initial
      _ <- runReaderT steps st
      freezeState st
