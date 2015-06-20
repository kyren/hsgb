{-# LANGUAGE QuasiQuotes #-}

module ProgramSpec (spec) where

import Data.Word
import Test.Hspec
import Gameboy.Assembler
import Gameboy.TestState
import Text.Heredoc

type MemCheck = [(Word16, Word8)]
type ProgramTest = (String, String, MemCheck)

testNoOps :: ProgramTest
testNoOps = (
    "runs no ops correctly",
    [here|
      NOP
      NOP
      NOP
      NOP
      STOP
    |],
    [(0xff, 0)]
  )

test8BitLoads :: ProgramTest
test8BitLoads = (
    "does 8 bit loads properly",
    [here|
      LD B,$07
      LD H,$00
      LD L,$f8
      LD (HL),B
      STOP
    |],
    [(0xf7, 0), (0xf8, 7), (0xf9, 0)]
  )

spec :: Spec
spec = describe "emulation" $ mapM_ runTest [testNoOps, test8BitLoads]
  where
    runTest (desc, asm, checks) = it desc $
      case assemble asm of
        Right prog -> testProgram prog checks `shouldBe` True
        Left er -> error er
