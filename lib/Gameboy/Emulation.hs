{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Gameboy.Emulation where

import Data.Word
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Control.Applicative
import Control.Monad.ST
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.STRef
import Gameboy.CPU
-- import Gameboy.Instructions

data SimpleGameboyState = SimpleGameboyState {
    memory :: VU.Vector Word8,
    aRegister, bRegister, cRegister, dRegister, eRegister, hRegister, lRegister, fRegister :: Word8,
    programCounter :: Word16,
    stackPointer :: Word16,
    clock :: Word64
  }

data SimpleGameboyStateST s = SimpleGameboyStateST {
    stMemory :: VUM.MVector s Word8,
    stARegister, stBRegister, stCRegister, stDRegister, stERegister, stHRegister, stLRegister, stFRegister :: STRef s Word8,
    stProgramCounter :: STRef s Word16,
    stStackPointer :: STRef s Word16,
    stClock :: STRef s Word64
  }

newtype SimpleGameboyST s a = SimpleGameboyST (ReaderT (SimpleGameboyStateST s) (ST s) a)
  deriving (Monad, Applicative, Functor)

freezeGameboyState :: SimpleGameboyStateST s -> ST s SimpleGameboyState
freezeGameboyState st = SimpleGameboyState <$>
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

thawGameboyState :: SimpleGameboyState -> ST s (SimpleGameboyStateST s)
thawGameboyState st = SimpleGameboyStateST <$>
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

instance Memory (SimpleGameboyST s) where
  getMemory addr = SimpleGameboyST $ do
    state <- ask
    lift $ VUM.read (stMemory state) (fromIntegral addr)
  setMemory addr byte = SimpleGameboyST $ do
    state <- ask
    lift $ VUM.write (stMemory state) (fromIntegral addr) byte

registerRef :: Register -> SimpleGameboyStateST s -> STRef s Word8
registerRef ARegister = stARegister
registerRef BRegister = stBRegister
registerRef CRegister = stCRegister
registerRef DRegister = stDRegister
registerRef ERegister = stERegister
registerRef HRegister = stHRegister
registerRef LRegister = stLRegister
registerRef FRegister = stFRegister

readRef :: (SimpleGameboyStateST s -> STRef s v) -> SimpleGameboyST s v
readRef field = SimpleGameboyST $ do
  state <- ask
  lift $ readSTRef (field state)

writeRef :: (SimpleGameboyStateST s -> STRef s v) -> v -> SimpleGameboyST s ()
writeRef field v = SimpleGameboyST $ do
  state <- ask
  lift $ writeSTRef (field state) v

instance CPU (SimpleGameboyST s) where
  getRegister r = readRef (registerRef r)
  setRegister r = writeRef (registerRef r)

  getProgramCounter = readRef stProgramCounter
  setProgramCounter = writeRef stProgramCounter

  getStackPointer = readRef stStackPointer
  setStackPointer = writeRef stStackPointer

  tick n = do
    c <- readRef stClock
    writeRef stClock (c + fromIntegral n)
