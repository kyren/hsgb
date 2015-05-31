module Gameboy.AssemblerSpec (spec) where

import Data.Either
import Test.Hspec
import Gameboy.Instructions
import Gameboy.Assembler

spec :: Spec
spec = describe "instruction emulation" $ do
    it "encodes 8 bit loads" $ do
      parseInstructions "LD B,255; comment\n" `shouldBe` Right [Load8I Load8B 255]
      parseInstructions "LD     C, (HL)\n" `shouldBe` Right [Load8 Load8C Load8AtHL]
    it "does not encode invalid instructions" $
      isLeft (parseInstructions "LD (HL),(HL)\n") `shouldBe` True

