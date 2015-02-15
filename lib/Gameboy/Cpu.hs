module Gameboy.CPU (
  CPU(..),
  InstructionInput(..),
  InstructionResult(..),
  reset,
  pushBC,
  popHL,
  loadA
) where

import Data.Word
import Data.Bits

data CPU = CPU {
    aRegister, bRegister, cRegister, dRegister, eRegister, hRegister, lRegister :: Word8,
    flags :: Word8,
    programCounter :: Word16,
    stackPointer :: Word16,
    mClock :: Integer,
    tClock :: Integer
  }

data InstructionInput = InstructionInput {
    cpuInState :: CPU,
    memoryAccess :: Word16 -> Word8
  }

data InstructionResult = InstructionResult {
    cpuOutState :: CPU,
    memoryModifications :: [(Word16, Word8)]
  }

reset :: CPU
reset = CPU 0 0 0 0 0 0 0 0 0 0 0 0

pushBC :: InstructionInput -> InstructionResult
pushBC (InstructionInput cpuIn _) = InstructionResult cpuOut memResult
  where
    cpuOut = cpuIn {
        stackPointer = stackPointer cpuIn - 2,
        mClock = mClock cpuIn + 3,
        tClock = tClock cpuIn + 12
      }
    memResult = [
        (stackPointer cpuIn - 1, bRegister cpuIn),
        (stackPointer cpuIn - 2, cRegister cpuIn)
      ]

popHL :: InstructionInput -> InstructionResult
popHL (InstructionInput cpuIn memAccess) = InstructionResult cpuOut memResult
  where
    cpuOut = cpuIn {
        stackPointer = stackPointer cpuIn + 2,
        hRegister = memAccess (stackPointer cpuIn + 1),
        lRegister = memAccess (stackPointer cpuIn),
        mClock = mClock cpuIn + 3,
        tClock = tClock cpuIn + 12
      }
    memResult = []

loadA :: InstructionInput -> InstructionResult
loadA (InstructionInput cpuIn memAccess) = InstructionResult cpuOut memResult
  where
    addr = readWord memAccess (programCounter cpuIn)
    cpuOut = cpuIn {
        aRegister = memAccess addr,
        programCounter = programCounter cpuIn + 2,
        mClock = mClock cpuIn + 4,
        tClock = tClock cpuIn + 16
      }
    memResult = []

readWord :: (Word16 -> Word8) -> Word16 -> Word16
readWord rd addr = shift high 8 .|. low
  where
    low = fromIntegral $ rd addr
    high = fromIntegral $ rd (addr + 1)
