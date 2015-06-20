{-# LANGUAGE QuasiQuotes #-}

module Gameboy.AssemblerSpec (spec) where

import Data.Either
import Test.Hspec
import Gameboy.Instructions
import Gameboy.Assembler
import Text.Heredoc

assemblyMatches :: String -> [Instruction] -> Expectation
assemblyMatches asm inst =
  parseInstructions asm `shouldBe` Right inst

instructionMatches :: String -> Instruction -> Expectation
instructionMatches asm inst = assemblyMatches asm [inst]

assemblyFails :: String -> Expectation
assemblyFails asm = parseInstructions asm `shouldSatisfy` isLeft

testParsing :: Spec
testParsing = it "handles blank lines and comments and spacing" $
  assemblyMatches
    [here|
      LD B,$ff; comment

      LD     C, (HL)
      ; comment
    |]
    [
      LD_R_N BRegister 0xff,
      LD_R_ATHL CRegister
    ]

testLoad8 :: Spec
testLoad8 = it "parses 8 bit loads" $ do
  instructionMatches "LD D, C" $ LD_R_R DRegister CRegister
  instructionMatches "LD B, $ff" $ LD_R_N BRegister 0xff
  instructionMatches "LD C, (HL)" $ LD_R_ATHL CRegister
  instructionMatches "LD (HL), E" $ LD_ATHL_R ERegister
  instructionMatches "LD (HL), $0a" $ LD_ATHL_N 0x0a
  instructionMatches "LD A, (C)" LD_A_ATC
  instructionMatches "LD A, (BC)" LD_A_ATBC
  instructionMatches "LD A, (DE)" LD_A_ATDE
  instructionMatches "LD A, ($f00f)" $ LD_A_ATNN 0xf00f
  instructionMatches "LD (C), A" LD_ATC_A
  instructionMatches "LD (BC), A" LD_ATBC_A
  instructionMatches "LD (DE), A" LD_ATDE_A
  instructionMatches "LD ($d00d), A" $ LD_ATNN_A 0xd00d
  assemblyFails "LD (HL), (HL)"
  assemblyFails "LD C, (DE)"
  assemblyFails "LD C, ($ffff)"

spec :: Spec
spec = describe "assembly" $ do
  testParsing
  testLoad8
