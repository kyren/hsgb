module Gameboy.AssemblerSpec (spec) where

import Data.Either
import Test.Hspec
import Gameboy.Instructions
import Gameboy.Assembler

spec :: Spec
spec = describe "instruction emulation" $ do
    it "parses 8 bit loads" $ do
      parseInstructions "LD B,$ff; comment" `shouldBe` Right [LD_R_N BRegister 255]
      parseInstructions "LD     C, (HL)\n\n; comment" `shouldBe` Right [LD_R_ATHL CRegister]
    it "does not parse invalid instructions" $
      isLeft (parseInstructions "LD (HL),(HL)") `shouldBe` True

