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
  instructionMatches "LDD A, (HL)" LDD_A_ATHL
  instructionMatches "LDD (HL), A" LDD_ATHL_A
  instructionMatches "LDI A, (HL)" LDI_A_ATHL
  instructionMatches "LDI (HL), A" LDI_ATHL_A
  instructionMatches "LDH A, ($fe)" $ LDH_A_ATN 0xfe
  instructionMatches "LDH ($ef), A" $ LDH_ATN_A 0xef
  assemblyFails "LD (HL), (HL)"
  assemblyFails "LD C, (DE)"
  assemblyFails "LD C, ($ffff)"

testLoad16 :: Spec
testLoad16 = it "parses 16 bit loads" $ do
  instructionMatches "LD BC, $feee" $ LD_BC_NN 0xfeee
  instructionMatches "LD DE, $f111" $ LD_DE_NN 0xf111
  instructionMatches "LD HL, $f000" $ LD_HL_NN 0xf000
  instructionMatches "LD SP, $faaa" $ LD_SP_NN 0xfaaa
  instructionMatches "LD SP, HL" LD_SP_HL
  instructionMatches "LDHL SP, $ab" $ LDHL_SP_N 0xab
  instructionMatches "LD ($0123), SP" $ LD_ATNN_SP 0x0123
  assemblyFails "LDHL SP, $abcd"

testPushPop :: Spec
testPushPop = it "parses push and pop instructions" $ do
  instructionMatches "PUSH AF" PUSH_AF
  instructionMatches "PUSH BC" PUSH_BC
  instructionMatches "POP DE" POP_DE
  instructionMatches "POP HL" POP_HL

testAddSub :: Spec
testAddSub = it "parses add / sub instructions" $ do
  instructionMatches "ADD A, A" $ ADD_A_R ARegister
  instructionMatches "ADC A, C" $ ADC_A_R CRegister
  instructionMatches "ADD A, $be" $ ADD_A_N 0xbe
  instructionMatches "ADC A, (HL)" ADC_A_ATHL
  instructionMatches "SBC A, B" $ SBC_A_R BRegister
  instructionMatches "SBC A, L" $ SBC_A_R LRegister
  instructionMatches "SUB $be" $ SUB_N 0xbe
  instructionMatches "SUB (HL)" SUB_ATHL
  instructionMatches "ADD HL, BC" ADD_HL_BC
  instructionMatches "ADD HL, SP" ADD_HL_SP
  instructionMatches "ADD SP, $51" $ ADD_SP_N 0x51
  assemblyFails "ADD SP, $f00d"
  assemblyFails "SUB HL, BC"
  assemblyFails "SBC HL, SP"
  assemblyFails "ADC SP, $51"

testAndOrXor :: Spec
testAndOrXor = it "parses and / or / xor instructions" $ do
  instructionMatches "AND B" $ AND_R BRegister
  instructionMatches "OR L" $ OR_R LRegister
  instructionMatches "AND $bb" $ AND_N 0xbb
  instructionMatches "XOR $00" $ XOR_N 0x00
  instructionMatches "AND (HL)" AND_ATHL
  instructionMatches "OR A" $ OR_R ARegister

testCp :: Spec
testCp = it "parses cp instructions" $ do
  instructionMatches "CP B" $ CP_R BRegister
  instructionMatches "CP $ee" $ CP_N 0xee
  instructionMatches "CP (HL)" CP_ATHL

testIncDec :: Spec
testIncDec = it "parses inc / dec instructions" $ do
  instructionMatches "INC C" $ INC_R CRegister
  instructionMatches "INC BC" INC_BC
  instructionMatches "INC SP" INC_SP
  instructionMatches "INC (HL)" INC_ATHL
  instructionMatches "DEC H" $ DEC_R HRegister
  instructionMatches "DEC (HL)" DEC_ATHL
  instructionMatches "DEC HL" DEC_HL
  instructionMatches "DEC SP" DEC_SP

testSwap :: Spec
testSwap = it "parses swap instructions" $ do
  instructionMatches "SWAP C" $ SWAP_R CRegister
  instructionMatches "SWAP (HL)" SWAP_ATHL

testMiscNoArg :: Spec
testMiscNoArg = it "parses misc no arg instructions" $ do
  instructionMatches "DAA" DAA
  instructionMatches "CPL" CPL
  instructionMatches "CCF" CCF
  instructionMatches "SCF" SCF
  instructionMatches "NOP" NOP
  instructionMatches "HALT" HALT
  instructionMatches "STOP" STOP
  instructionMatches "DI" DI
  instructionMatches "EI" EI
  instructionMatches "RLCA" RLCA
  instructionMatches "RLA" RLA
  instructionMatches "RRCA" RRCA
  instructionMatches "RRA" RRA

testRotatesShifts :: Spec
testRotatesShifts = it "parses rotate and shift instructions" $ do
  instructionMatches "RLC B" $ RLC_R BRegister
  instructionMatches "RLC (HL)" RLC_ATHL
  instructionMatches "RL D" $ RL_R DRegister
  instructionMatches "RL (HL)" RL_ATHL
  instructionMatches "RRC C" $ RRC_R CRegister
  instructionMatches "RRC (HL)" RRC_ATHL
  instructionMatches "RR E" $ RR_R ERegister
  instructionMatches "RR (HL)" RR_ATHL
  instructionMatches "SLA B" $ SLA_R BRegister
  instructionMatches "SLA (HL)" SLA_ATHL
  instructionMatches "SRA L" $ SRA_R LRegister
  instructionMatches "SRA (HL)" SRA_ATHL
  instructionMatches "SRL H" $ SRL_R HRegister
  instructionMatches "SRL (HL)" SRL_ATHL

testBitOps :: Spec
testBitOps = it "parses bit set and res instructions" $ do
  instructionMatches "BIT 7, B" $ BIT_B_R Bit7 BRegister
  instructionMatches "BIT 0, (HL)" $ BIT_B_ATHL Bit0
  instructionMatches "SET 6, B" $ SET_B_R Bit6 BRegister
  instructionMatches "SET 1, (HL)" $ SET_B_ATHL Bit1
  instructionMatches "RES 5, B" $ RES_B_R Bit5 BRegister
  instructionMatches "RES 2, (HL)" $ RES_B_ATHL Bit2

testJumps :: Spec
testJumps = it "parses jp and jr instructions" $ do
  instructionMatches "JP $dead" $ JP_NN 0xdead
  instructionMatches "JP NZ, $beef" $ JP_C_NN NZero 0xbeef
  instructionMatches "JP C, $f00f" $ JP_C_NN Carry 0xf00f
  instructionMatches "JR Z, $00" $ JR_C_N Zero 0x00
  instructionMatches "JR $be" $ JR_N 0xbe
  instructionMatches "JR NC, $ef" $ JR_C_N NCarry 0xef

testCall :: Spec
testCall = it "parses call instructions" $ do
  instructionMatches "CALL $f00f" $ CALL_NN 0xf00f
  instructionMatches "CALL NC, $f00f" $ CALL_C_NN NCarry 0xf00f
  instructionMatches "CALL C, $f00f" $ CALL_C_NN Carry 0xf00f
  instructionMatches "CALL NZ, $f00f" $ CALL_C_NN NZero 0xf00f
  instructionMatches "CALL Z, $f00f" $ CALL_C_NN Zero 0xf00f

testRst :: Spec
testRst = it "parses rst instruction" $ do
  instructionMatches "RST $38" $ RST_RA Reset38
  instructionMatches "RST $08" $ RST_RA Reset08
  assemblyFails "RST $07"
  assemblyFails "RST $37"

testReturns :: Spec
testReturns = it "parses ret / reti instructions" $ do
  instructionMatches "RET" RET
  instructionMatches "RET NZ" $ RET_C NZero
  instructionMatches "RETI" RETI

spec :: Spec
spec = describe "assembly" $ do
  testParsing
  testLoad8
  testLoad16
  testPushPop
  testAddSub
  testAndOrXor
  testCp
  testIncDec
  testSwap
  testMiscNoArg
  testRotatesShifts
  testBitOps
  testJumps
  testCall
  testRst
  testReturns
