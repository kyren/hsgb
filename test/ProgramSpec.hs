module ProgramSpec (spec) where

import Data.Word
import Test.Hspec
import Gameboy.Assembler
import Gameboy.TestState

testAssembly :: String -> [(Word16, Word8)] -> Bool
testAssembly assembly = case assemble assembly of
                      Right prog -> testProgram prog
                      Left er -> error er

spec :: Spec
spec = describe "emulation" $ do
    it "runs no-ops correctly" $
      testAssembly "NOP\nSTOP\n" [(0xff, 0)] `shouldBe` True
    it "runs 8-bit loads correctly" $
      testAssembly "\
      \ LD B,$07\n\
      \ LD H,$00\n\
      \ LD L,$f8\n\
      \ LD (HL),B\n\
      \ STOP\n"
      [(0xf7, 0), (0xf8, 7), (0xf9, 0)] `shouldBe` True
