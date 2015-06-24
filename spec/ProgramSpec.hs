{-# LANGUAGE QuasiQuotes #-}

module ProgramSpec (spec) where

import Data.Word
import Test.Hspec
import Gameboy.Assembler
import Gameboy.TestState
import Text.Heredoc

runTest :: String -> [(Word16, Word8)] -> Expectation
runTest asm checks =
  case assemble asm of
    Right prog -> testProgram prog checks `shouldBe` True
    Left er -> error er

testNoOps :: Spec
testNoOps = it "runs no ops correctly" $
  runTest [here|
      NOP
      NOP
      NOP
      NOP
      STOP
    |] [(0xff, 0)]

test8BitLoads :: Spec
test8BitLoads = it "does 8 bit loads properly" $
  runTest [here|
      LD B,$07
      LD H,$00
      LD L,$f8
      LD (HL),B
      STOP
    |] [(0xf7, 0), (0xf8, 7), (0xf9, 0)]

spec :: Spec
spec = describe "emulation" $ do
  testNoOps
  test8BitLoads
