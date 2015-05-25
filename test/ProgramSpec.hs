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
      testAssembly "NOP\nSTOP\n" [(100, 0)] `shouldBe` True
    it "runs 8-bit loads correctly" $
      testAssembly "\
      \ LD B,7\n\
      \ LD H,0\n\
      \ LD L,200\n\
      \ LD (HL),B\n\
      \ STOP\n"
      [(199, 0), (200, 7), (201, 0)] `shouldBe` True
