{-# LANGUAGE QuasiQuotes #-}

module Gameboy.AssemblerSpec (spec) where

import Data.Either
import Test.Hspec
import Gameboy.Instructions
import Gameboy.Assembler
import Text.Heredoc

testLoad8 :: Spec
testLoad8 = it "parses 8 bit loads" $
  parseInstructions
    [here|
      LD B,$ff; comment
      LD     C, (HL)
      ; comment
    |] `shouldBe` Right [
      LD_R_N BRegister 0xff,
      LD_R_ATHL CRegister
    ]

testInvalidLoad8 :: Spec
testInvalidLoad8 = it "does not parse invalid 8 bit loads" $
  parseInstructions "LD (HL), (HL)" `shouldSatisfy` isLeft

spec :: Spec
spec = describe "assembly" $ do
  testLoad8
  testInvalidLoad8
