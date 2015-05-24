module ProgramSpec (spec) where

import Data.Word
import Test.Hspec
import Gameboy.Assembler
import Gameboy.SimpleState

testAssembly :: String -> Int -> [(Word16, Word8)] -> Bool
testAssembly assembly = case assemble assembly of
                      Right prog -> testProgram prog
                      Left er -> error er

spec :: Spec
spec = describe "emulation" $ do
    it "runs no-ops correctly" $
      testAssembly "NOP\n" 100 [(0, 0), (1, 0), (2, 0)] `shouldBe` True
    it "runs 8-bit loads correctly" $
      testAssembly "\
      \ LD B,7\n\
      \ LD H,0\n\
      \ LD L,200\n\
      \ LD (HL),B\n"
      4 [(200, 7)] `shouldBe` True
