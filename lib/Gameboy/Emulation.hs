module Gameboy.Emulation (
  step
) where

import Data.Word
import Gameboy.Util
import Gameboy.CPU
import Gameboy.Instructions

step :: (CPU m, Memory m) => m ()
step = do
  mi <- decodeInstruction getNextPC
  case mi of
    Just i -> doInstruction i
    Nothing -> fail "invalid opcode"

doInstruction :: (CPU m, Memory m) => Instruction -> m ()
doInstruction NoOp = tick 1
doInstruction Stop = stop >> tick 4
-- TODO: timing unimplemented here
doInstruction (Load8 t s) = getLoad8Part s >>= setLoad8Part t >> tick 0

getLoad8Part :: (CPU m, Memory m) => Load8Part -> m Word8
getLoad8Part Load8A = getARegister
getLoad8Part Load8B = getBRegister
getLoad8Part Load8C = getCRegister
getLoad8Part Load8D = getDRegister
getLoad8Part Load8E = getERegister
getLoad8Part Load8H = getHRegister
getLoad8Part Load8L = getLRegister
getLoad8Part Load8AtBC = do
  b <- getBRegister
  c <- getCRegister
  getMemory (makeWord c b)
getLoad8Part Load8AtDE = do
  d <- getDRegister
  e <- getERegister
  getMemory (makeWord e d)
getLoad8Part Load8AtHL = do
  h <- getHRegister
  l <- getLRegister
  getMemory (makeWord l h)
getLoad8Part (Load8AtNN n) = getMemory n
getLoad8Part (Load8I n) = return n

setLoad8Part :: (CPU m, Memory m) => Load8Part -> Word8 -> m ()
setLoad8Part Load8A = setARegister
setLoad8Part Load8B = setBRegister
setLoad8Part Load8C = setCRegister
setLoad8Part Load8D = setDRegister
setLoad8Part Load8E = setERegister
setLoad8Part Load8H = setHRegister
setLoad8Part Load8L = setLRegister
setLoad8Part Load8AtBC = \v -> do
  b <- getBRegister
  c <- getCRegister
  setMemory (makeWord c b) v
setLoad8Part Load8AtDE = \v -> do
  d <- getDRegister
  e <- getERegister
  setMemory (makeWord e d) v
setLoad8Part Load8AtHL = \v -> do
  h <- getHRegister
  l <- getLRegister
  setMemory (makeWord l h) v
setLoad8Part (Load8AtNN nn) = setMemory nn
setLoad8Part (Load8I _) = fail "invalid operation"

getNextPC :: (CPU m, Memory m) => m Word8
getNextPC = do
  pc <- getProgramCounter
  setProgramCounter (pc + 1)
  getMemory pc
