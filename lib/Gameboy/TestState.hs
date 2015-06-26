{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Gameboy.TestState (
  testProgram
) where

import Data.Word
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.STRef
import Gameboy.CPU
import Gameboy.Emulation

-- Runs a gameboy cpu ish program starting at 0x0 instead of 0x100 and stops at
-- the first STOP instruction
data TestState = TestState {
    memory :: VU.Vector Word8,
    aRegister, bRegister, cRegister, dRegister, eRegister, hRegister, lRegister, fRegister :: Word8,
    programCounter :: Word16,
    stackPointer :: Word16,
    clock :: Word64,
    stopped :: Bool
  }
  deriving Show

data TestStateST s = TestStateST {
    stMemory :: VUM.MVector s Word8,
    stARegister, stBRegister, stCRegister, stDRegister, stERegister, stHRegister, stLRegister, stFRegister :: STRef s Word8,
    stProgramCounter :: STRef s Word16,
    stStackPointer :: STRef s Word16,
    stClock :: STRef s Word64,
    stStopped :: STRef s Bool
  }

newtype TestEnvironmentST s a = TestEnvironmentST {
    runTestEnvironmentST :: ReaderT (TestStateST s) (ST s) a
  } deriving (Monad, Applicative, Functor)

freezeTestState :: TestStateST s -> ST s TestState
freezeTestState st = TestState <$>
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
    rd stClock <*>
    rd stStopped
  where
    rd f = readSTRef (f st)

thawTestState :: TestState -> ST s (TestStateST s)
thawTestState st = TestStateST <$>
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
    mk clock <*>
    mk stopped
  where
    mk f = newSTRef (f st)

instance Memory (TestEnvironmentST s) where
  getMemory addr = TestEnvironmentST $ do
    state <- ask
    lift $ VUM.read (stMemory state) (fromIntegral addr)
  setMemory addr byte = TestEnvironmentST $ do
    state <- ask
    lift $ VUM.write (stMemory state) (fromIntegral addr) byte

readRef :: (TestStateST s -> STRef s v) -> TestEnvironmentST s v
readRef field = TestEnvironmentST $ do
  state <- ask
  lift $ readSTRef (field state)

writeRef :: (TestStateST s -> STRef s v) -> v -> TestEnvironmentST s ()
writeRef field v = TestEnvironmentST $ do
  state <- ask
  lift $ writeSTRef (field state) v

instance CPU (TestEnvironmentST s) where
  getARegister = readRef stARegister
  setARegister = writeRef stARegister

  getBRegister = readRef stBRegister
  setBRegister = writeRef stBRegister

  getCRegister = readRef stCRegister
  setCRegister = writeRef stCRegister

  getDRegister = readRef stDRegister
  setDRegister = writeRef stDRegister

  getERegister = readRef stERegister
  setERegister = writeRef stERegister

  getHRegister = readRef stHRegister
  setHRegister = writeRef stHRegister

  getLRegister = readRef stLRegister
  setLRegister = writeRef stLRegister

  getFRegister = readRef stFRegister
  setFRegister = writeRef stFRegister

  getProgramCounter = readRef stProgramCounter
  setProgramCounter = writeRef stProgramCounter

  getStackPointer = readRef stStackPointer
  setStackPointer = writeRef stStackPointer

  halt = return ()

  stop = writeRef stStopped True

  tick n = do
    c <- readRef stClock
    writeRef stClock (c + fromIntegral n)

runTestState :: TestState -> TestState
runTestState initial =
    runST $ do
      tstate <- thawTestState initial
      (runReaderT . runTestEnvironmentST) go tstate
      freezeTestState tstate
  where
    go = do
      isStopped <- readRef stStopped
      unless isStopped (step >> go)

initialTestState :: VU.Vector Word8 -> TestState
initialTestState is =
    TestState (makelength is 0x10000) 0 0 0 0 0 0 0 0 0 0 0 False
  where
    makelength v s =
      VU.take s v VU.++ VU.replicate (max (s - VU.length v) 0) 0x0

testProgram :: VU.Vector Word8 -> [(Word16, Word8)] -> Bool
testProgram prog tests = and [check a v | (a, v) <- tests]
  where
    state = runTestState (initialTestState prog)
    check a v = (memory state VU.! fromIntegral a) == v
