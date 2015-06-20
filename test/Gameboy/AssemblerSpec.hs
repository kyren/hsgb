{-# LANGUAGE QuasiQuotes #-}

module Gameboy.AssemblerSpec (spec) where

import Data.Either
import Test.Hspec
import Gameboy.Instructions
import Gameboy.Assembler
import Text.Heredoc

type AssemblyPassTest = (String, String, [Instruction])
type AssemblyFailTest = (String, [String])

testLoad8 :: AssemblyPassTest
testLoad8 = (
    "parses 8 bit loads",
    [here|
      LD B,$ff; comment
      LD     C, (HL)
      ; comment
    |],
    [
      LD_R_N BRegister 0xff,
      LD_R_ATHL CRegister
    ]
  )

testInvalidLoad8 :: AssemblyFailTest
testInvalidLoad8 = (
    "does not parse invalid 8 bit loads",
    [
      "LD (HL), (HL)"
    ]
  )

spec :: Spec
spec = describe "assembly" $ do
    mapM_ runPassTest [testLoad8]
    mapM_ runFailTest [testInvalidLoad8]
  where
    runPassTest (desc, asm, inst) = it desc $ parseInstructions asm `shouldBe` Right inst
    runFailTest (desc, asms) = it desc $ all isLeft $ map parseInstructions asms
