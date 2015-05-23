module Gameboy.AssemblerSpec (spec) where

import Test.Hspec
import Gameboy.Assembler
import Gameboy.CPU
import Gameboy.Instructions

spec :: Spec
spec = do
  describe "parseInstructions" $ do
    it "parses assembly string into a list of instructions" $ do
      parseInstructions "LD A,255; comment\n" `shouldBe` Right [Load8I ARegister 255]

