module Gameboy.Instructions (
  Bit(..),
  Register(..),
  Cond(..),
  ResetAddress(..),
  Instruction(..),
  encodeInstruction,
  decodeInstruction
) where

import Data.Word
import Gameboy.Util

data Bit
  = Bit0
  | Bit1
  | Bit2
  | Bit3
  | Bit4
  | Bit5
  | Bit6
  | Bit7
  deriving (Show, Eq)

data Register
  = ARegister
  | BRegister
  | CRegister
  | DRegister
  | ERegister
  | HRegister
  | LRegister
  deriving (Show, Eq)

data Cond
  = Zero
  | NZero
  | Carry
  | NCarry
  deriving (Show, Eq)

data ResetAddress
  = Reset00
  | Reset08
  | Reset10
  | Reset18
  | Reset20
  | Reset28
  | Reset30
  | Reset38
  deriving (Show, Eq)

data Instruction
  = LD_R_R Register Register
  | LD_R_N Register Word8
  | LD_R_ATHL Register

  | LD_ATHL_R Register
  | LD_ATHL_N Word8

  | LD_A_ATC
  | LD_A_ATBC
  | LD_A_ATDE
  | LD_A_ATNN Word16

  | LD_ATC_A
  | LD_ATBC_A
  | LD_ATDE_A
  | LD_ATNN_A Word16

  | LDD_A_ATHL
  | LDD_ATHL_A

  | LDI_A_ATHL
  | LDI_ATHL_A

  | LDH_A_ATN Word8
  | LDH_ATN_A Word8

  | LD_BC_NN Word16
  | LD_DE_NN Word16
  | LD_HL_NN Word16
  | LD_SP_NN Word16

  | LD_SP_HL
  | LDHL_SP_N Word8
  | LD_ATNN_SP Word16

  | PUSH_AF
  | PUSH_BC
  | PUSH_DE
  | PUSH_HL

  | POP_AF
  | POP_BC
  | POP_DE
  | POP_HL

  | ADD_A_R Register
  | ADD_A_N Word8
  | ADD_A_ATHL

  | ADC_A_R Register
  | ADC_A_N Word8
  | ADC_A_ATHL

  | SUB_R Register
  | SUB_N Word8
  | SUB_ATHL

  | SBC_A_R Register
  | SBC_A_N Word8
  | SBC_A_ATHL

  | AND_R Register
  | AND_N Word8
  | AND_ATHL

  | OR_R Register
  | OR_ATHL
  | OR_N Word8

  | XOR_R Register
  | XOR_N Word8
  | XOR_ATHL

  | CP_R Register
  | CP_N Word8
  | CP_ATHL

  | INC_R Register
  | INC_ATHL

  | DEC_R Register
  | DEC_ATHL

  | ADD_HL_BC
  | ADD_HL_DE
  | ADD_HL_HL
  | ADD_HL_SP

  | ADD_SP_N Word8

  | INC_BC
  | INC_DE
  | INC_HL
  | INC_SP

  | DEC_BC
  | DEC_DE
  | DEC_HL
  | DEC_SP

  | SWAP_R Register
  | SWAP_ATHL

  | DAA
  | CPL
  | CCF
  | SCF

  | NOP
  | HALT
  | STOP
  | DI
  | EI

  | RLCA
  | RLA
  | RRCA
  | RRA

  | RLC_R Register
  | RLC_ATHL

  | RL_R Register
  | RL_ATHL

  | RRC_R Register
  | RRC_ATHL

  | RR_R Register
  | RR_ATHL

  | SLA_R Register
  | SLA_ATHL

  | SRA_R Register
  | SRA_ATHL

  | SRL_R Register
  | SRL_ATHL

  | BIT_B_R Bit Register
  | BIT_B_ATHL Bit

  | SET_B_R Bit Register
  | SET_B_ATHL Bit

  | RES_B_R Bit Register
  | RES_B_ATHL Bit

  | JP_NN Word16
  | JP_C_NN Cond Word16
  | JP_ATHL

  | JR_N Word8
  | JR_C_N Cond Word8

  | CALL_NN Word16
  | CALL_C_NN Cond Word16

  | RST_RA ResetAddress

  | RET
  | RET_C Cond

  | RETI

  deriving (Show, Eq)

encodeInstruction :: Instruction -> [Word8]

encodeInstruction (LD_R_R ARegister ARegister) = [0x7f]
encodeInstruction (LD_R_R ARegister BRegister) = [0x78]
encodeInstruction (LD_R_R ARegister CRegister) = [0x79]
encodeInstruction (LD_R_R ARegister DRegister) = [0x7a]
encodeInstruction (LD_R_R ARegister ERegister) = [0x7b]
encodeInstruction (LD_R_R ARegister HRegister) = [0x7c]
encodeInstruction (LD_R_R ARegister LRegister) = [0x7d]

encodeInstruction (LD_R_R BRegister ARegister) = [0x47]
encodeInstruction (LD_R_R BRegister BRegister) = [0x40]
encodeInstruction (LD_R_R BRegister CRegister) = [0x41]
encodeInstruction (LD_R_R BRegister DRegister) = [0x42]
encodeInstruction (LD_R_R BRegister ERegister) = [0x43]
encodeInstruction (LD_R_R BRegister HRegister) = [0x44]
encodeInstruction (LD_R_R BRegister LRegister) = [0x45]

encodeInstruction (LD_R_R CRegister ARegister) = [0x4f]
encodeInstruction (LD_R_R CRegister BRegister) = [0x48]
encodeInstruction (LD_R_R CRegister CRegister) = [0x49]
encodeInstruction (LD_R_R CRegister DRegister) = [0x4a]
encodeInstruction (LD_R_R CRegister ERegister) = [0x4b]
encodeInstruction (LD_R_R CRegister HRegister) = [0x4c]
encodeInstruction (LD_R_R CRegister LRegister) = [0x4d]

encodeInstruction (LD_R_R DRegister ARegister) = [0x57]
encodeInstruction (LD_R_R DRegister BRegister) = [0x50]
encodeInstruction (LD_R_R DRegister CRegister) = [0x51]
encodeInstruction (LD_R_R DRegister DRegister) = [0x52]
encodeInstruction (LD_R_R DRegister ERegister) = [0x53]
encodeInstruction (LD_R_R DRegister HRegister) = [0x54]
encodeInstruction (LD_R_R DRegister LRegister) = [0x55]

encodeInstruction (LD_R_R ERegister ARegister) = [0x5f]
encodeInstruction (LD_R_R ERegister BRegister) = [0x58]
encodeInstruction (LD_R_R ERegister CRegister) = [0x59]
encodeInstruction (LD_R_R ERegister DRegister) = [0x5a]
encodeInstruction (LD_R_R ERegister ERegister) = [0x5b]
encodeInstruction (LD_R_R ERegister HRegister) = [0x5c]
encodeInstruction (LD_R_R ERegister LRegister) = [0x5d]

encodeInstruction (LD_R_R HRegister ARegister) = [0x67]
encodeInstruction (LD_R_R HRegister BRegister) = [0x60]
encodeInstruction (LD_R_R HRegister CRegister) = [0x61]
encodeInstruction (LD_R_R HRegister DRegister) = [0x62]
encodeInstruction (LD_R_R HRegister ERegister) = [0x63]
encodeInstruction (LD_R_R HRegister HRegister) = [0x64]
encodeInstruction (LD_R_R HRegister LRegister) = [0x65]

encodeInstruction (LD_R_R LRegister ARegister) = [0x6f]
encodeInstruction (LD_R_R LRegister BRegister) = [0x68]
encodeInstruction (LD_R_R LRegister CRegister) = [0x69]
encodeInstruction (LD_R_R LRegister DRegister) = [0x6a]
encodeInstruction (LD_R_R LRegister ERegister) = [0x6b]
encodeInstruction (LD_R_R LRegister HRegister) = [0x6c]
encodeInstruction (LD_R_R LRegister LRegister) = [0x6d]

encodeInstruction (LD_R_N ARegister n) = [0x3e, n]
encodeInstruction (LD_R_N BRegister n) = [0x06, n]
encodeInstruction (LD_R_N CRegister n) = [0x0e, n]
encodeInstruction (LD_R_N DRegister n) = [0x16, n]
encodeInstruction (LD_R_N ERegister n) = [0x1e, n]
encodeInstruction (LD_R_N HRegister n) = [0x26, n]
encodeInstruction (LD_R_N LRegister n) = [0x2e, n]

encodeInstruction (LD_R_ATHL ARegister) = [0x7e]
encodeInstruction (LD_R_ATHL BRegister) = [0x46]
encodeInstruction (LD_R_ATHL CRegister) = [0x4e]
encodeInstruction (LD_R_ATHL DRegister) = [0x56]
encodeInstruction (LD_R_ATHL ERegister) = [0x5e]
encodeInstruction (LD_R_ATHL HRegister) = [0x66]
encodeInstruction (LD_R_ATHL LRegister) = [0x6e]

encodeInstruction (LD_ATHL_R ARegister) = [0x77]
encodeInstruction (LD_ATHL_R BRegister) = [0x70]
encodeInstruction (LD_ATHL_R CRegister) = [0x71]
encodeInstruction (LD_ATHL_R DRegister) = [0x72]
encodeInstruction (LD_ATHL_R ERegister) = [0x73]
encodeInstruction (LD_ATHL_R HRegister) = [0x74]
encodeInstruction (LD_ATHL_R LRegister) = [0x75]

encodeInstruction (LD_ATHL_N n) = [0x36, n]

encodeInstruction LD_A_ATC = [0xf2]
encodeInstruction LD_A_ATBC = [0x0a]
encodeInstruction LD_A_ATDE = [0x1a]
encodeInstruction (LD_A_ATNN nn) = [0xfa, lowByte nn, highByte nn]

encodeInstruction LD_ATC_A = [0xe2]
encodeInstruction LD_ATBC_A = [0x02]
encodeInstruction LD_ATDE_A = [0x12]
encodeInstruction (LD_ATNN_A nn) = [0xea, lowByte nn, highByte nn]

encodeInstruction LDD_A_ATHL = [0x3a]
encodeInstruction LDD_ATHL_A = [0x32]

encodeInstruction LDI_A_ATHL = [0x2a]
encodeInstruction LDI_ATHL_A = [0x22]

encodeInstruction (LDH_A_ATN n) = [0xf0, n]
encodeInstruction (LDH_ATN_A n) = [0xe0, n]

encodeInstruction (LD_BC_NN nn) = [0x01, lowByte nn, highByte nn]
encodeInstruction (LD_DE_NN nn) = [0x11, lowByte nn, highByte nn]
encodeInstruction (LD_HL_NN nn) = [0x21, lowByte nn, highByte nn]
encodeInstruction (LD_SP_NN nn) = [0x31, lowByte nn, highByte nn]

encodeInstruction LD_SP_HL = [0xf9]
encodeInstruction (LDHL_SP_N n) = [0xf8, n]
encodeInstruction (LD_ATNN_SP nn) = [0x08, lowByte nn, highByte nn]

encodeInstruction PUSH_AF = [0xf5]
encodeInstruction PUSH_BC = [0xc5]
encodeInstruction PUSH_DE = [0xd5]
encodeInstruction PUSH_HL = [0xe5]

encodeInstruction POP_AF = [0xf1]
encodeInstruction POP_BC = [0xc1]
encodeInstruction POP_DE = [0xd1]
encodeInstruction POP_HL = [0xe1]

encodeInstruction (ADD_A_R ARegister) = [0x87]
encodeInstruction (ADD_A_R BRegister) = [0x80]
encodeInstruction (ADD_A_R CRegister) = [0x81]
encodeInstruction (ADD_A_R DRegister) = [0x82]
encodeInstruction (ADD_A_R ERegister) = [0x83]
encodeInstruction (ADD_A_R HRegister) = [0x84]
encodeInstruction (ADD_A_R LRegister) = [0x85]
encodeInstruction (ADD_A_N n) = [0xc6, n]
encodeInstruction ADD_A_ATHL = [0x86]

encodeInstruction (ADC_A_R ARegister) = [0x8f]
encodeInstruction (ADC_A_R BRegister) = [0x88]
encodeInstruction (ADC_A_R CRegister) = [0x89]
encodeInstruction (ADC_A_R DRegister) = [0x8a]
encodeInstruction (ADC_A_R ERegister) = [0x8b]
encodeInstruction (ADC_A_R HRegister) = [0x8c]
encodeInstruction (ADC_A_R LRegister) = [0x8d]
encodeInstruction (ADC_A_N n) = [0xce, n]
encodeInstruction ADC_A_ATHL = [0x8e]

encodeInstruction (SUB_R ARegister) = [0x97]
encodeInstruction (SUB_R BRegister) = [0x90]
encodeInstruction (SUB_R CRegister) = [0x91]
encodeInstruction (SUB_R DRegister) = [0x92]
encodeInstruction (SUB_R ERegister) = [0x93]
encodeInstruction (SUB_R HRegister) = [0x94]
encodeInstruction (SUB_R LRegister) = [0x95]
encodeInstruction (SUB_N n) = [0xd6, n]
encodeInstruction SUB_ATHL = [0x96]

encodeInstruction (SBC_A_R ARegister) = [0x9f]
encodeInstruction (SBC_A_R BRegister) = [0x98]
encodeInstruction (SBC_A_R CRegister) = [0x99]
encodeInstruction (SBC_A_R DRegister) = [0x9a]
encodeInstruction (SBC_A_R ERegister) = [0x9b]
encodeInstruction (SBC_A_R HRegister) = [0x9c]
encodeInstruction (SBC_A_R LRegister) = [0x9d]
encodeInstruction (SBC_A_N n) = [0xde, n]
encodeInstruction SBC_A_ATHL = [0x9e]

encodeInstruction (AND_R ARegister) = [0xa7]
encodeInstruction (AND_R BRegister) = [0xa0]
encodeInstruction (AND_R CRegister) = [0xa1]
encodeInstruction (AND_R DRegister) = [0xa2]
encodeInstruction (AND_R ERegister) = [0xa3]
encodeInstruction (AND_R HRegister) = [0xa4]
encodeInstruction (AND_R LRegister) = [0xa5]
encodeInstruction (AND_N n) = [0xe6, n]
encodeInstruction AND_ATHL = [0xa6]

encodeInstruction (OR_R ARegister) = [0xb7]
encodeInstruction (OR_R BRegister) = [0xb0]
encodeInstruction (OR_R CRegister) = [0xb1]
encodeInstruction (OR_R DRegister) = [0xb2]
encodeInstruction (OR_R ERegister) = [0xb3]
encodeInstruction (OR_R HRegister) = [0xb4]
encodeInstruction (OR_R LRegister) = [0xb5]
encodeInstruction (OR_N n) = [0xf6, n]
encodeInstruction OR_ATHL = [0xb6]

encodeInstruction (XOR_R ARegister) = [0xaf]
encodeInstruction (XOR_R BRegister) = [0xa8]
encodeInstruction (XOR_R CRegister) = [0xa9]
encodeInstruction (XOR_R DRegister) = [0xaa]
encodeInstruction (XOR_R ERegister) = [0xab]
encodeInstruction (XOR_R HRegister) = [0xac]
encodeInstruction (XOR_R LRegister) = [0xad]
encodeInstruction (XOR_N n) = [0xee, n]
encodeInstruction XOR_ATHL = [0xae]

encodeInstruction (CP_R ARegister) = [0xbf]
encodeInstruction (CP_R BRegister) = [0xb8]
encodeInstruction (CP_R CRegister) = [0xb9]
encodeInstruction (CP_R DRegister) = [0xba]
encodeInstruction (CP_R ERegister) = [0xbb]
encodeInstruction (CP_R HRegister) = [0xbc]
encodeInstruction (CP_R LRegister) = [0xbd]
encodeInstruction (CP_N n) = [0xfe, n]
encodeInstruction CP_ATHL = [0xbe]

encodeInstruction (INC_R ARegister) = [0x3c]
encodeInstruction (INC_R BRegister) = [0x04]
encodeInstruction (INC_R CRegister) = [0x0c]
encodeInstruction (INC_R DRegister) = [0x14]
encodeInstruction (INC_R ERegister) = [0x1c]
encodeInstruction (INC_R HRegister) = [0x24]
encodeInstruction (INC_R LRegister) = [0x2c]
encodeInstruction INC_ATHL = [0x34]

encodeInstruction (DEC_R ARegister) = [0x3d]
encodeInstruction (DEC_R BRegister) = [0x05]
encodeInstruction (DEC_R CRegister) = [0x0d]
encodeInstruction (DEC_R DRegister) = [0x15]
encodeInstruction (DEC_R ERegister) = [0x1d]
encodeInstruction (DEC_R HRegister) = [0x25]
encodeInstruction (DEC_R LRegister) = [0x2d]
encodeInstruction DEC_ATHL = [0x35]

encodeInstruction ADD_HL_BC = [0x09]
encodeInstruction ADD_HL_DE = [0x19]
encodeInstruction ADD_HL_HL = [0x29]
encodeInstruction ADD_HL_SP = [0x39]

encodeInstruction (ADD_SP_N n) = [0xe8, n]

encodeInstruction INC_BC = [0x03]
encodeInstruction INC_DE = [0x13]
encodeInstruction INC_HL = [0x23]
encodeInstruction INC_SP = [0x33]

encodeInstruction DEC_BC = [0x0b]
encodeInstruction DEC_DE = [0x1b]
encodeInstruction DEC_HL = [0x2b]
encodeInstruction DEC_SP = [0x3b]

encodeInstruction (SWAP_R ARegister) = [0xcb, 0x37]
encodeInstruction (SWAP_R BRegister) = [0xcb, 0x30]
encodeInstruction (SWAP_R CRegister) = [0xcb, 0x31]
encodeInstruction (SWAP_R DRegister) = [0xcb, 0x32]
encodeInstruction (SWAP_R ERegister) = [0xcb, 0x33]
encodeInstruction (SWAP_R HRegister) = [0xcb, 0x34]
encodeInstruction (SWAP_R LRegister) = [0xcb, 0x35]
encodeInstruction SWAP_ATHL = [0xcb, 0x36]

encodeInstruction DAA = [0x27]
encodeInstruction CPL = [0x2f]
encodeInstruction CCF = [0x3f]
encodeInstruction SCF = [0x37]

encodeInstruction NOP = [0x00]
encodeInstruction HALT = [0x76]
encodeInstruction STOP = [0x10, 0x00]
encodeInstruction DI = [0xf3]
encodeInstruction EI = [0xfb]

encodeInstruction RLCA = [0x07]
encodeInstruction RLA = [0x17]
encodeInstruction RRCA = [0x0f]
encodeInstruction RRA = [0x1f]

encodeInstruction (RLC_R ARegister) = [0xcb, 0x07]
encodeInstruction (RLC_R BRegister) = [0xcb, 0x00]
encodeInstruction (RLC_R CRegister) = [0xcb, 0x01]
encodeInstruction (RLC_R DRegister) = [0xcb, 0x02]
encodeInstruction (RLC_R ERegister) = [0xcb, 0x03]
encodeInstruction (RLC_R HRegister) = [0xcb, 0x04]
encodeInstruction (RLC_R LRegister) = [0xcb, 0x05]
encodeInstruction RLC_ATHL = [0xcb, 0x06]

encodeInstruction (RL_R ARegister) = [0xcb, 0x17]
encodeInstruction (RL_R BRegister) = [0xcb, 0x10]
encodeInstruction (RL_R CRegister) = [0xcb, 0x11]
encodeInstruction (RL_R DRegister) = [0xcb, 0x12]
encodeInstruction (RL_R ERegister) = [0xcb, 0x13]
encodeInstruction (RL_R HRegister) = [0xcb, 0x14]
encodeInstruction (RL_R LRegister) = [0xcb, 0x15]
encodeInstruction RL_ATHL = [0xcb, 0x16]

encodeInstruction (RRC_R ARegister) = [0xcb, 0x0f]
encodeInstruction (RRC_R BRegister) = [0xcb, 0x08]
encodeInstruction (RRC_R CRegister) = [0xcb, 0x09]
encodeInstruction (RRC_R DRegister) = [0xcb, 0x0a]
encodeInstruction (RRC_R ERegister) = [0xcb, 0x0b]
encodeInstruction (RRC_R HRegister) = [0xcb, 0x0c]
encodeInstruction (RRC_R LRegister) = [0xcb, 0x0d]
encodeInstruction RRC_ATHL = [0xcb, 0x0e]

encodeInstruction (RR_R ARegister) = [0xcb, 0x1f]
encodeInstruction (RR_R BRegister) = [0xcb, 0x18]
encodeInstruction (RR_R CRegister) = [0xcb, 0x19]
encodeInstruction (RR_R DRegister) = [0xcb, 0x1a]
encodeInstruction (RR_R ERegister) = [0xcb, 0x1b]
encodeInstruction (RR_R HRegister) = [0xcb, 0x1c]
encodeInstruction (RR_R LRegister) = [0xcb, 0x1d]
encodeInstruction RR_ATHL = [0xcb, 0x1e]

encodeInstruction (SLA_R ARegister) = [0xcb, 0x27]
encodeInstruction (SLA_R BRegister) = [0xcb, 0x20]
encodeInstruction (SLA_R CRegister) = [0xcb, 0x21]
encodeInstruction (SLA_R DRegister) = [0xcb, 0x22]
encodeInstruction (SLA_R ERegister) = [0xcb, 0x23]
encodeInstruction (SLA_R HRegister) = [0xcb, 0x24]
encodeInstruction (SLA_R LRegister) = [0xcb, 0x25]
encodeInstruction SLA_ATHL = [0xcb, 0x26]

encodeInstruction (SRA_R ARegister) = [0xcb, 0x2f]
encodeInstruction (SRA_R BRegister) = [0xcb, 0x28]
encodeInstruction (SRA_R CRegister) = [0xcb, 0x29]
encodeInstruction (SRA_R DRegister) = [0xcb, 0x2a]
encodeInstruction (SRA_R ERegister) = [0xcb, 0x2b]
encodeInstruction (SRA_R HRegister) = [0xcb, 0x2c]
encodeInstruction (SRA_R LRegister) = [0xcb, 0x2d]
encodeInstruction SRA_ATHL = [0xcb, 0x2e]

encodeInstruction (SRL_R ARegister) = [0xcb, 0x3f]
encodeInstruction (SRL_R BRegister) = [0xcb, 0x38]
encodeInstruction (SRL_R CRegister) = [0xcb, 0x39]
encodeInstruction (SRL_R DRegister) = [0xcb, 0x3a]
encodeInstruction (SRL_R ERegister) = [0xcb, 0x3b]
encodeInstruction (SRL_R HRegister) = [0xcb, 0x3c]
encodeInstruction (SRL_R LRegister) = [0xcb, 0x3d]
encodeInstruction SRL_ATHL = [0xcb, 0x3e]

encodeInstruction (BIT_B_R Bit0 ARegister) = [0xcb, 0x47]
encodeInstruction (BIT_B_R Bit0 BRegister) = [0xcb, 0x40]
encodeInstruction (BIT_B_R Bit0 CRegister) = [0xcb, 0x41]
encodeInstruction (BIT_B_R Bit0 DRegister) = [0xcb, 0x42]
encodeInstruction (BIT_B_R Bit0 ERegister) = [0xcb, 0x43]
encodeInstruction (BIT_B_R Bit0 HRegister) = [0xcb, 0x44]
encodeInstruction (BIT_B_R Bit0 LRegister) = [0xcb, 0x45]
encodeInstruction (BIT_B_ATHL Bit0) = [0xcb, 0x46]

encodeInstruction (BIT_B_R Bit1 ARegister) = [0xcb, 0x48]
encodeInstruction (BIT_B_R Bit1 BRegister) = [0xcb, 0x49]
encodeInstruction (BIT_B_R Bit1 CRegister) = [0xcb, 0x4a]
encodeInstruction (BIT_B_R Bit1 DRegister) = [0xcb, 0x4b]
encodeInstruction (BIT_B_R Bit1 ERegister) = [0xcb, 0x4c]
encodeInstruction (BIT_B_R Bit1 HRegister) = [0xcb, 0x4d]
encodeInstruction (BIT_B_R Bit1 LRegister) = [0xcb, 0x4e]
encodeInstruction (BIT_B_ATHL Bit1) = [0xcb, 0x4f]

encodeInstruction (BIT_B_R Bit2 ARegister) = [0xcb, 0x57]
encodeInstruction (BIT_B_R Bit2 BRegister) = [0xcb, 0x50]
encodeInstruction (BIT_B_R Bit2 CRegister) = [0xcb, 0x51]
encodeInstruction (BIT_B_R Bit2 DRegister) = [0xcb, 0x52]
encodeInstruction (BIT_B_R Bit2 ERegister) = [0xcb, 0x53]
encodeInstruction (BIT_B_R Bit2 HRegister) = [0xcb, 0x54]
encodeInstruction (BIT_B_R Bit2 LRegister) = [0xcb, 0x55]
encodeInstruction (BIT_B_ATHL Bit2) = [0xcb, 0x56]

encodeInstruction (BIT_B_R Bit3 ARegister) = [0xcb, 0x58]
encodeInstruction (BIT_B_R Bit3 BRegister) = [0xcb, 0x59]
encodeInstruction (BIT_B_R Bit3 CRegister) = [0xcb, 0x5a]
encodeInstruction (BIT_B_R Bit3 DRegister) = [0xcb, 0x5b]
encodeInstruction (BIT_B_R Bit3 ERegister) = [0xcb, 0x5c]
encodeInstruction (BIT_B_R Bit3 HRegister) = [0xcb, 0x5d]
encodeInstruction (BIT_B_R Bit3 LRegister) = [0xcb, 0x5e]
encodeInstruction (BIT_B_ATHL Bit3) = [0xcb, 0x5f]

encodeInstruction (BIT_B_R Bit4 ARegister) = [0xcb, 0x67]
encodeInstruction (BIT_B_R Bit4 BRegister) = [0xcb, 0x60]
encodeInstruction (BIT_B_R Bit4 CRegister) = [0xcb, 0x61]
encodeInstruction (BIT_B_R Bit4 DRegister) = [0xcb, 0x62]
encodeInstruction (BIT_B_R Bit4 ERegister) = [0xcb, 0x63]
encodeInstruction (BIT_B_R Bit4 HRegister) = [0xcb, 0x64]
encodeInstruction (BIT_B_R Bit4 LRegister) = [0xcb, 0x65]
encodeInstruction (BIT_B_ATHL Bit4) = [0xcb, 0x66]

encodeInstruction (BIT_B_R Bit5 ARegister) = [0xcb, 0x68]
encodeInstruction (BIT_B_R Bit5 BRegister) = [0xcb, 0x69]
encodeInstruction (BIT_B_R Bit5 CRegister) = [0xcb, 0x6a]
encodeInstruction (BIT_B_R Bit5 DRegister) = [0xcb, 0x6b]
encodeInstruction (BIT_B_R Bit5 ERegister) = [0xcb, 0x6c]
encodeInstruction (BIT_B_R Bit5 HRegister) = [0xcb, 0x6d]
encodeInstruction (BIT_B_R Bit5 LRegister) = [0xcb, 0x6e]
encodeInstruction (BIT_B_ATHL Bit5) = [0xcb, 0x6f]

encodeInstruction (BIT_B_R Bit6 ARegister) = [0xcb, 0x77]
encodeInstruction (BIT_B_R Bit6 BRegister) = [0xcb, 0x70]
encodeInstruction (BIT_B_R Bit6 CRegister) = [0xcb, 0x71]
encodeInstruction (BIT_B_R Bit6 DRegister) = [0xcb, 0x72]
encodeInstruction (BIT_B_R Bit6 ERegister) = [0xcb, 0x73]
encodeInstruction (BIT_B_R Bit6 HRegister) = [0xcb, 0x74]
encodeInstruction (BIT_B_R Bit6 LRegister) = [0xcb, 0x75]
encodeInstruction (BIT_B_ATHL Bit6) = [0xcb, 0x76]

encodeInstruction (BIT_B_R Bit7 ARegister) = [0xcb, 0x78]
encodeInstruction (BIT_B_R Bit7 BRegister) = [0xcb, 0x79]
encodeInstruction (BIT_B_R Bit7 CRegister) = [0xcb, 0x7a]
encodeInstruction (BIT_B_R Bit7 DRegister) = [0xcb, 0x7b]
encodeInstruction (BIT_B_R Bit7 ERegister) = [0xcb, 0x7c]
encodeInstruction (BIT_B_R Bit7 HRegister) = [0xcb, 0x7d]
encodeInstruction (BIT_B_R Bit7 LRegister) = [0xcb, 0x7e]
encodeInstruction (BIT_B_ATHL Bit7) = [0xcb, 0x7f]

encodeInstruction (SET_B_R Bit0 ARegister) = [0xcb, 0xc7]
encodeInstruction (SET_B_R Bit0 BRegister) = [0xcb, 0xc0]
encodeInstruction (SET_B_R Bit0 CRegister) = [0xcb, 0xc1]
encodeInstruction (SET_B_R Bit0 DRegister) = [0xcb, 0xc2]
encodeInstruction (SET_B_R Bit0 ERegister) = [0xcb, 0xc3]
encodeInstruction (SET_B_R Bit0 HRegister) = [0xcb, 0xc4]
encodeInstruction (SET_B_R Bit0 LRegister) = [0xcb, 0xc5]
encodeInstruction (SET_B_ATHL Bit0) = [0xcb, 0xc6]

encodeInstruction (SET_B_R Bit1 ARegister) = [0xcb, 0xc8]
encodeInstruction (SET_B_R Bit1 BRegister) = [0xcb, 0xc9]
encodeInstruction (SET_B_R Bit1 CRegister) = [0xcb, 0xca]
encodeInstruction (SET_B_R Bit1 DRegister) = [0xcb, 0xcb]
encodeInstruction (SET_B_R Bit1 ERegister) = [0xcb, 0xcc]
encodeInstruction (SET_B_R Bit1 HRegister) = [0xcb, 0xcd]
encodeInstruction (SET_B_R Bit1 LRegister) = [0xcb, 0xce]
encodeInstruction (SET_B_ATHL Bit1) = [0xcb, 0xcf]

encodeInstruction (SET_B_R Bit2 ARegister) = [0xcb, 0xd7]
encodeInstruction (SET_B_R Bit2 BRegister) = [0xcb, 0xd0]
encodeInstruction (SET_B_R Bit2 CRegister) = [0xcb, 0xd1]
encodeInstruction (SET_B_R Bit2 DRegister) = [0xcb, 0xd2]
encodeInstruction (SET_B_R Bit2 ERegister) = [0xcb, 0xd3]
encodeInstruction (SET_B_R Bit2 HRegister) = [0xcb, 0xd4]
encodeInstruction (SET_B_R Bit2 LRegister) = [0xcb, 0xd5]
encodeInstruction (SET_B_ATHL Bit2) = [0xcb, 0xd6]

encodeInstruction (SET_B_R Bit3 ARegister) = [0xcb, 0xd8]
encodeInstruction (SET_B_R Bit3 BRegister) = [0xcb, 0xd9]
encodeInstruction (SET_B_R Bit3 CRegister) = [0xcb, 0xda]
encodeInstruction (SET_B_R Bit3 DRegister) = [0xcb, 0xdb]
encodeInstruction (SET_B_R Bit3 ERegister) = [0xcb, 0xdc]
encodeInstruction (SET_B_R Bit3 HRegister) = [0xcb, 0xdd]
encodeInstruction (SET_B_R Bit3 LRegister) = [0xcb, 0xde]
encodeInstruction (SET_B_ATHL Bit3) = [0xcb, 0xdf]

encodeInstruction (SET_B_R Bit4 ARegister) = [0xcb, 0xe7]
encodeInstruction (SET_B_R Bit4 BRegister) = [0xcb, 0xe0]
encodeInstruction (SET_B_R Bit4 CRegister) = [0xcb, 0xe1]
encodeInstruction (SET_B_R Bit4 DRegister) = [0xcb, 0xe2]
encodeInstruction (SET_B_R Bit4 ERegister) = [0xcb, 0xe3]
encodeInstruction (SET_B_R Bit4 HRegister) = [0xcb, 0xe4]
encodeInstruction (SET_B_R Bit4 LRegister) = [0xcb, 0xe5]
encodeInstruction (SET_B_ATHL Bit4) = [0xcb, 0xe6]

encodeInstruction (SET_B_R Bit5 ARegister) = [0xcb, 0xe8]
encodeInstruction (SET_B_R Bit5 BRegister) = [0xcb, 0xe9]
encodeInstruction (SET_B_R Bit5 CRegister) = [0xcb, 0xea]
encodeInstruction (SET_B_R Bit5 DRegister) = [0xcb, 0xeb]
encodeInstruction (SET_B_R Bit5 ERegister) = [0xcb, 0xec]
encodeInstruction (SET_B_R Bit5 HRegister) = [0xcb, 0xed]
encodeInstruction (SET_B_R Bit5 LRegister) = [0xcb, 0xee]
encodeInstruction (SET_B_ATHL Bit5) = [0xcb, 0xef]

encodeInstruction (SET_B_R Bit6 ARegister) = [0xcb, 0xf7]
encodeInstruction (SET_B_R Bit6 BRegister) = [0xcb, 0xf0]
encodeInstruction (SET_B_R Bit6 CRegister) = [0xcb, 0xf1]
encodeInstruction (SET_B_R Bit6 DRegister) = [0xcb, 0xf2]
encodeInstruction (SET_B_R Bit6 ERegister) = [0xcb, 0xf3]
encodeInstruction (SET_B_R Bit6 HRegister) = [0xcb, 0xf4]
encodeInstruction (SET_B_R Bit6 LRegister) = [0xcb, 0xf5]
encodeInstruction (SET_B_ATHL Bit6) = [0xcb, 0xf6]

encodeInstruction (SET_B_R Bit7 ARegister) = [0xcb, 0xf8]
encodeInstruction (SET_B_R Bit7 BRegister) = [0xcb, 0xf9]
encodeInstruction (SET_B_R Bit7 CRegister) = [0xcb, 0xfa]
encodeInstruction (SET_B_R Bit7 DRegister) = [0xcb, 0xfb]
encodeInstruction (SET_B_R Bit7 ERegister) = [0xcb, 0xfc]
encodeInstruction (SET_B_R Bit7 HRegister) = [0xcb, 0xfd]
encodeInstruction (SET_B_R Bit7 LRegister) = [0xcb, 0xfe]
encodeInstruction (SET_B_ATHL Bit7) = [0xcb, 0xff]

encodeInstruction (RES_B_R Bit0 ARegister) = [0xcb, 0x87]
encodeInstruction (RES_B_R Bit0 BRegister) = [0xcb, 0x80]
encodeInstruction (RES_B_R Bit0 CRegister) = [0xcb, 0x81]
encodeInstruction (RES_B_R Bit0 DRegister) = [0xcb, 0x82]
encodeInstruction (RES_B_R Bit0 ERegister) = [0xcb, 0x83]
encodeInstruction (RES_B_R Bit0 HRegister) = [0xcb, 0x84]
encodeInstruction (RES_B_R Bit0 LRegister) = [0xcb, 0x85]
encodeInstruction (RES_B_ATHL Bit0) = [0xcb, 0x86]

encodeInstruction (RES_B_R Bit1 ARegister) = [0xcb, 0x88]
encodeInstruction (RES_B_R Bit1 BRegister) = [0xcb, 0x89]
encodeInstruction (RES_B_R Bit1 CRegister) = [0xcb, 0x8a]
encodeInstruction (RES_B_R Bit1 DRegister) = [0xcb, 0x8b]
encodeInstruction (RES_B_R Bit1 ERegister) = [0xcb, 0x8c]
encodeInstruction (RES_B_R Bit1 HRegister) = [0xcb, 0x8d]
encodeInstruction (RES_B_R Bit1 LRegister) = [0xcb, 0x8e]
encodeInstruction (RES_B_ATHL Bit1) = [0xcb, 0x8f]

encodeInstruction (RES_B_R Bit2 ARegister) = [0xcb, 0x97]
encodeInstruction (RES_B_R Bit2 BRegister) = [0xcb, 0x90]
encodeInstruction (RES_B_R Bit2 CRegister) = [0xcb, 0x91]
encodeInstruction (RES_B_R Bit2 DRegister) = [0xcb, 0x92]
encodeInstruction (RES_B_R Bit2 ERegister) = [0xcb, 0x93]
encodeInstruction (RES_B_R Bit2 HRegister) = [0xcb, 0x94]
encodeInstruction (RES_B_R Bit2 LRegister) = [0xcb, 0x95]
encodeInstruction (RES_B_ATHL Bit2) = [0xcb, 0x96]

encodeInstruction (RES_B_R Bit3 ARegister) = [0xcb, 0x98]
encodeInstruction (RES_B_R Bit3 BRegister) = [0xcb, 0x99]
encodeInstruction (RES_B_R Bit3 CRegister) = [0xcb, 0x9a]
encodeInstruction (RES_B_R Bit3 DRegister) = [0xcb, 0x9b]
encodeInstruction (RES_B_R Bit3 ERegister) = [0xcb, 0x9c]
encodeInstruction (RES_B_R Bit3 HRegister) = [0xcb, 0x9d]
encodeInstruction (RES_B_R Bit3 LRegister) = [0xcb, 0x9e]
encodeInstruction (RES_B_ATHL Bit3) = [0xcb, 0x9f]

encodeInstruction (RES_B_R Bit4 ARegister) = [0xcb, 0xa7]
encodeInstruction (RES_B_R Bit4 BRegister) = [0xcb, 0xa0]
encodeInstruction (RES_B_R Bit4 CRegister) = [0xcb, 0xa1]
encodeInstruction (RES_B_R Bit4 DRegister) = [0xcb, 0xa2]
encodeInstruction (RES_B_R Bit4 ERegister) = [0xcb, 0xa3]
encodeInstruction (RES_B_R Bit4 HRegister) = [0xcb, 0xa4]
encodeInstruction (RES_B_R Bit4 LRegister) = [0xcb, 0xa5]
encodeInstruction (RES_B_ATHL Bit4) = [0xcb, 0xa6]

encodeInstruction (RES_B_R Bit5 ARegister) = [0xcb, 0xa8]
encodeInstruction (RES_B_R Bit5 BRegister) = [0xcb, 0xa9]
encodeInstruction (RES_B_R Bit5 CRegister) = [0xcb, 0xaa]
encodeInstruction (RES_B_R Bit5 DRegister) = [0xcb, 0xab]
encodeInstruction (RES_B_R Bit5 ERegister) = [0xcb, 0xac]
encodeInstruction (RES_B_R Bit5 HRegister) = [0xcb, 0xad]
encodeInstruction (RES_B_R Bit5 LRegister) = [0xcb, 0xae]
encodeInstruction (RES_B_ATHL Bit5) = [0xcb, 0xaf]

encodeInstruction (RES_B_R Bit6 ARegister) = [0xcb, 0xb7]
encodeInstruction (RES_B_R Bit6 BRegister) = [0xcb, 0xb0]
encodeInstruction (RES_B_R Bit6 CRegister) = [0xcb, 0xb1]
encodeInstruction (RES_B_R Bit6 DRegister) = [0xcb, 0xb2]
encodeInstruction (RES_B_R Bit6 ERegister) = [0xcb, 0xb3]
encodeInstruction (RES_B_R Bit6 HRegister) = [0xcb, 0xb4]
encodeInstruction (RES_B_R Bit6 LRegister) = [0xcb, 0xb5]
encodeInstruction (RES_B_ATHL Bit6) = [0xcb, 0xb6]

encodeInstruction (RES_B_R Bit7 ARegister) = [0xcb, 0xb8]
encodeInstruction (RES_B_R Bit7 BRegister) = [0xcb, 0xb9]
encodeInstruction (RES_B_R Bit7 CRegister) = [0xcb, 0xba]
encodeInstruction (RES_B_R Bit7 DRegister) = [0xcb, 0xbb]
encodeInstruction (RES_B_R Bit7 ERegister) = [0xcb, 0xbc]
encodeInstruction (RES_B_R Bit7 HRegister) = [0xcb, 0xbd]
encodeInstruction (RES_B_R Bit7 LRegister) = [0xcb, 0xbe]
encodeInstruction (RES_B_ATHL Bit7) = [0xcb, 0xbf]

encodeInstruction (JP_NN nn) = [0xc3, lowByte nn, highByte nn]
encodeInstruction (JP_C_NN NZero nn) = [0xc2, lowByte nn, highByte nn]
encodeInstruction (JP_C_NN Zero nn) = [0xca, lowByte nn, highByte nn]
encodeInstruction (JP_C_NN NCarry nn) = [0xd2, lowByte nn, highByte nn]
encodeInstruction (JP_C_NN Carry nn) = [0xda, lowByte nn, highByte nn]
encodeInstruction JP_ATHL = [0xe9]

encodeInstruction (JR_N n) = [0x18, n]
encodeInstruction (JR_C_N NZero n) = [0x20, n]
encodeInstruction (JR_C_N Zero n) = [0x28, n]
encodeInstruction (JR_C_N NCarry n) = [0x30, n]
encodeInstruction (JR_C_N Carry n) = [0x38, n]

encodeInstruction (CALL_NN nn) = [0xcd, lowByte nn, highByte nn]
encodeInstruction (CALL_C_NN NZero nn) = [0xc4, lowByte nn, highByte nn]
encodeInstruction (CALL_C_NN Zero nn) = [0xcc, lowByte nn, highByte nn]
encodeInstruction (CALL_C_NN NCarry nn) = [0xd4, lowByte nn, highByte nn]
encodeInstruction (CALL_C_NN Carry nn) = [0xdc, lowByte nn, highByte nn]

encodeInstruction (RST_RA Reset00) = [0xc7]
encodeInstruction (RST_RA Reset08) = [0xcf]
encodeInstruction (RST_RA Reset10) = [0xd7]
encodeInstruction (RST_RA Reset18) = [0xdf]
encodeInstruction (RST_RA Reset20) = [0xe7]
encodeInstruction (RST_RA Reset28) = [0xef]
encodeInstruction (RST_RA Reset30) = [0xf7]
encodeInstruction (RST_RA Reset38) = [0xff]

encodeInstruction RET = [0xc9]
encodeInstruction (RET_C NZero) = [0xc0]
encodeInstruction (RET_C Zero) = [0xc8]
encodeInstruction (RET_C NCarry) = [0xd0]
encodeInstruction (RET_C Carry) = [0xd8]

encodeInstruction RETI = [0xd9]

decodeInstruction :: (Monad m) => m Word8 -> m (Maybe Instruction)
decodeInstruction getWord8 = do
    w <- getWord8
    case w of
      0x7f -> dec (LD_R_R ARegister ARegister)
      0x78 -> dec (LD_R_R ARegister BRegister)
      0x79 -> dec (LD_R_R ARegister CRegister)
      0x7a -> dec (LD_R_R ARegister DRegister)
      0x7b -> dec (LD_R_R ARegister ERegister)
      0x7c -> dec (LD_R_R ARegister HRegister)
      0x7d -> dec (LD_R_R ARegister LRegister)

      0x47 -> dec (LD_R_R BRegister ARegister)
      0x40 -> dec (LD_R_R BRegister BRegister)
      0x41 -> dec (LD_R_R BRegister CRegister)
      0x42 -> dec (LD_R_R BRegister DRegister)
      0x43 -> dec (LD_R_R BRegister ERegister)
      0x44 -> dec (LD_R_R BRegister HRegister)
      0x45 -> dec (LD_R_R BRegister LRegister)

      0x4f -> dec (LD_R_R CRegister ARegister)
      0x48 -> dec (LD_R_R CRegister BRegister)
      0x49 -> dec (LD_R_R CRegister CRegister)
      0x4a -> dec (LD_R_R CRegister DRegister)
      0x4b -> dec (LD_R_R CRegister ERegister)
      0x4c -> dec (LD_R_R CRegister HRegister)
      0x4d -> dec (LD_R_R CRegister LRegister)

      0x57 -> dec (LD_R_R DRegister ARegister)
      0x50 -> dec (LD_R_R DRegister BRegister)
      0x51 -> dec (LD_R_R DRegister CRegister)
      0x52 -> dec (LD_R_R DRegister DRegister)
      0x53 -> dec (LD_R_R DRegister ERegister)
      0x54 -> dec (LD_R_R DRegister HRegister)
      0x55 -> dec (LD_R_R DRegister LRegister)

      0x5f -> dec (LD_R_R ERegister ARegister)
      0x58 -> dec (LD_R_R ERegister BRegister)
      0x59 -> dec (LD_R_R ERegister CRegister)
      0x5a -> dec (LD_R_R ERegister DRegister)
      0x5b -> dec (LD_R_R ERegister ERegister)
      0x5c -> dec (LD_R_R ERegister HRegister)
      0x5d -> dec (LD_R_R ERegister LRegister)

      0x67 -> dec (LD_R_R HRegister ARegister)
      0x60 -> dec (LD_R_R HRegister BRegister)
      0x61 -> dec (LD_R_R HRegister CRegister)
      0x62 -> dec (LD_R_R HRegister DRegister)
      0x63 -> dec (LD_R_R HRegister ERegister)
      0x64 -> dec (LD_R_R HRegister HRegister)
      0x65 -> dec (LD_R_R HRegister LRegister)

      0x6f -> dec (LD_R_R LRegister ARegister)
      0x68 -> dec (LD_R_R LRegister BRegister)
      0x69 -> dec (LD_R_R LRegister CRegister)
      0x6a -> dec (LD_R_R LRegister DRegister)
      0x6b -> dec (LD_R_R LRegister ERegister)
      0x6c -> dec (LD_R_R LRegister HRegister)
      0x6d -> dec (LD_R_R LRegister LRegister)

      0x3e -> decn (LD_R_N ARegister)
      0x06 -> decn (LD_R_N BRegister)
      0x0e -> decn (LD_R_N CRegister)
      0x16 -> decn (LD_R_N DRegister)
      0x1e -> decn (LD_R_N ERegister)
      0x26 -> decn (LD_R_N HRegister)
      0x2e -> decn (LD_R_N LRegister)

      0x7e -> dec (LD_R_ATHL ARegister)
      0x46 -> dec (LD_R_ATHL BRegister)
      0x4e -> dec (LD_R_ATHL CRegister)
      0x56 -> dec (LD_R_ATHL DRegister)
      0x5e -> dec (LD_R_ATHL ERegister)
      0x66 -> dec (LD_R_ATHL HRegister)
      0x6e -> dec (LD_R_ATHL LRegister)

      0x77 -> dec (LD_ATHL_R ARegister)
      0x70 -> dec (LD_ATHL_R BRegister)
      0x71 -> dec (LD_ATHL_R CRegister)
      0x72 -> dec (LD_ATHL_R DRegister)
      0x73 -> dec (LD_ATHL_R ERegister)
      0x74 -> dec (LD_ATHL_R HRegister)
      0x75 -> dec (LD_ATHL_R LRegister)

      0x36 -> decn LD_ATHL_N

      0xf2 -> dec LD_A_ATC
      0x0a -> dec LD_A_ATBC
      0x1a -> dec LD_A_ATDE
      0xfa -> decnn LD_A_ATNN

      0xe2 -> dec LD_ATC_A
      0x02 -> dec LD_ATBC_A
      0x12 -> dec LD_ATDE_A
      0xea -> decnn LD_ATNN_A

      0x3a -> dec LDD_A_ATHL
      0x32 -> dec LDD_ATHL_A

      0x2a -> dec LDI_A_ATHL
      0x22 -> dec LDI_ATHL_A

      0xf0 -> decn LDH_A_ATN
      0xe0 -> decn LDH_ATN_A

      0x01 -> decnn LD_BC_NN
      0x11 -> decnn LD_DE_NN
      0x21 -> decnn LD_HL_NN
      0x31 -> decnn LD_SP_NN

      0xf9 -> dec LD_SP_HL
      0xf8 -> decn LDHL_SP_N
      0x08 -> decnn LD_ATNN_SP

      0xf5 -> dec PUSH_AF
      0xc5 -> dec PUSH_BC
      0xd5 -> dec PUSH_DE
      0xe5 -> dec PUSH_HL

      0xf1 -> dec POP_AF
      0xc1 -> dec POP_BC
      0xd1 -> dec POP_DE
      0xe1 -> dec POP_HL

      0x87 -> dec (ADD_A_R ARegister)
      0x80 -> dec (ADD_A_R BRegister)
      0x81 -> dec (ADD_A_R CRegister)
      0x82 -> dec (ADD_A_R DRegister)
      0x83 -> dec (ADD_A_R ERegister)
      0x84 -> dec (ADD_A_R HRegister)
      0x85 -> dec (ADD_A_R LRegister)
      0xc6 -> decn ADD_A_N
      0x86 -> dec ADD_A_ATHL

      0x8f -> dec (ADC_A_R ARegister)
      0x88 -> dec (ADC_A_R BRegister)
      0x89 -> dec (ADC_A_R CRegister)
      0x8a -> dec (ADC_A_R DRegister)
      0x8b -> dec (ADC_A_R ERegister)
      0x8c -> dec (ADC_A_R HRegister)
      0x8d -> dec (ADC_A_R LRegister)
      0xce -> decn ADC_A_N
      0x8e -> dec ADC_A_ATHL

      0x97 -> dec (SUB_R ARegister)
      0x90 -> dec (SUB_R BRegister)
      0x91 -> dec (SUB_R CRegister)
      0x92 -> dec (SUB_R DRegister)
      0x93 -> dec (SUB_R ERegister)
      0x94 -> dec (SUB_R HRegister)
      0x95 -> dec (SUB_R LRegister)
      0xd6 -> decn SUB_N
      0x96 -> dec SUB_ATHL

      0x9f -> dec (SBC_A_R ARegister)
      0x98 -> dec (SBC_A_R BRegister)
      0x99 -> dec (SBC_A_R CRegister)
      0x9a -> dec (SBC_A_R DRegister)
      0x9b -> dec (SBC_A_R ERegister)
      0x9c -> dec (SBC_A_R HRegister)
      0x9d -> dec (SBC_A_R LRegister)
      0xde -> decn SBC_A_N
      0x9e -> dec SBC_A_ATHL

      0xa7 -> dec (AND_R ARegister)
      0xa0 -> dec (AND_R BRegister)
      0xa1 -> dec (AND_R CRegister)
      0xa2 -> dec (AND_R DRegister)
      0xa3 -> dec (AND_R ERegister)
      0xa4 -> dec (AND_R HRegister)
      0xa5 -> dec (AND_R LRegister)
      0xe6 -> decn AND_N
      0xa6 -> dec AND_ATHL

      0xb7 -> dec (OR_R ARegister)
      0xb0 -> dec (OR_R BRegister)
      0xb1 -> dec (OR_R CRegister)
      0xb2 -> dec (OR_R DRegister)
      0xb3 -> dec (OR_R ERegister)
      0xb4 -> dec (OR_R HRegister)
      0xb5 -> dec (OR_R LRegister)
      0xf6 -> decn OR_N
      0xb6 -> dec OR_ATHL

      0xaf -> dec (XOR_R ARegister)
      0xa8 -> dec (XOR_R BRegister)
      0xa9 -> dec (XOR_R CRegister)
      0xaa -> dec (XOR_R DRegister)
      0xab -> dec (XOR_R ERegister)
      0xac -> dec (XOR_R HRegister)
      0xad -> dec (XOR_R LRegister)
      0xee -> decn XOR_N
      0xae -> dec XOR_ATHL

      0xbf -> dec (CP_R ARegister)
      0xb8 -> dec (CP_R BRegister)
      0xb9 -> dec (CP_R CRegister)
      0xba -> dec (CP_R DRegister)
      0xbb -> dec (CP_R ERegister)
      0xbc -> dec (CP_R HRegister)
      0xbd -> dec (CP_R LRegister)
      0xfe -> decn CP_N
      0xbe -> dec CP_ATHL

      0x3c -> dec (INC_R ARegister)
      0x04 -> dec (INC_R BRegister)
      0x0c -> dec (INC_R CRegister)
      0x14 -> dec (INC_R DRegister)
      0x1c -> dec (INC_R ERegister)
      0x24 -> dec (INC_R HRegister)
      0x2c -> dec (INC_R LRegister)
      0x34 -> dec INC_ATHL

      0x3d -> dec (DEC_R ARegister)
      0x05 -> dec (DEC_R BRegister)
      0x0d -> dec (DEC_R CRegister)
      0x15 -> dec (DEC_R DRegister)
      0x1d -> dec (DEC_R ERegister)
      0x25 -> dec (DEC_R HRegister)
      0x2d -> dec (DEC_R LRegister)
      0x35 -> dec DEC_ATHL

      0x09 -> dec ADD_HL_BC
      0x19 -> dec ADD_HL_DE
      0x29 -> dec ADD_HL_HL
      0x39 -> dec ADD_HL_SP

      0xe8 -> decn ADD_SP_N

      0x03 -> dec INC_BC
      0x13 -> dec INC_DE
      0x23 -> dec INC_HL
      0x33 -> dec INC_SP

      0x0b -> dec DEC_BC
      0x1b -> dec DEC_DE
      0x2b -> dec DEC_HL
      0x3b -> dec DEC_SP

      0x27 -> dec DAA
      0x2f -> dec CPL
      0x3f -> dec CCF
      0x37 -> dec SCF

      0x00 -> dec NOP
      0x76 -> dec HALT

      0xf3 -> dec DI
      0xfb -> dec EI

      0x07 -> dec RLCA
      0x17 -> dec RLA
      0x0f -> dec RRCA
      0x1f -> dec RRA

      0xc3 -> decnn JP_NN
      0xc2 -> decnn (JP_C_NN NZero)
      0xca -> decnn (JP_C_NN Zero)
      0xd2 -> decnn (JP_C_NN NCarry)
      0xda -> decnn (JP_C_NN Carry)
      0xe9 -> dec JP_ATHL 

      0x18 -> decn JR_N
      0x20 -> decn (JR_C_N NZero)
      0x28 -> decn (JR_C_N Zero)
      0x30 -> decn (JR_C_N NCarry)
      0x38 -> decn (JR_C_N Carry)

      0xcd -> decnn CALL_NN
      0xc4 -> decnn (CALL_C_NN NZero)
      0xcc -> decnn (CALL_C_NN Zero)
      0xd4 -> decnn (CALL_C_NN NCarry)
      0xdc -> decnn (CALL_C_NN Carry)

      0xc7 -> dec (RST_RA Reset00)
      0xcf -> dec (RST_RA Reset08)
      0xd7 -> dec (RST_RA Reset10)
      0xdf -> dec (RST_RA Reset18)
      0xe7 -> dec (RST_RA Reset20)
      0xef -> dec (RST_RA Reset28)
      0xf7 -> dec (RST_RA Reset30)
      0xff -> dec (RST_RA Reset38)

      0xc9 -> dec RET
      0xc0 -> dec (RET_C NZero)
      0xc8 -> dec (RET_C Zero)
      0xd0 -> dec (RET_C NCarry)
      0xd8 -> dec (RET_C Carry)

      0xd9 -> dec RETI

      0x10 -> do
        w2 <- getWord8 
        case w2 of
          0x00 -> dec STOP
          _ -> return Nothing

      0xcb -> do
        w2 <- getWord8
        case w2 of
          0x37 -> dec (SWAP_R ARegister)
          0x30 -> dec (SWAP_R BRegister)
          0x31 -> dec (SWAP_R CRegister)
          0x32 -> dec (SWAP_R DRegister)
          0x33 -> dec (SWAP_R ERegister)
          0x34 -> dec (SWAP_R HRegister)
          0x35 -> dec (SWAP_R LRegister)
          0x36 -> dec SWAP_ATHL

          0x07 -> dec (RLC_R ARegister)
          0x00 -> dec (RLC_R BRegister)
          0x01 -> dec (RLC_R CRegister)
          0x02 -> dec (RLC_R DRegister)
          0x03 -> dec (RLC_R ERegister)
          0x04 -> dec (RLC_R HRegister)
          0x05 -> dec (RLC_R LRegister)
          0x06 -> dec RLC_ATHL

          0x17 -> dec (RL_R ARegister)
          0x10 -> dec (RL_R BRegister)
          0x11 -> dec (RL_R CRegister)
          0x12 -> dec (RL_R DRegister)
          0x13 -> dec (RL_R ERegister)
          0x14 -> dec (RL_R HRegister)
          0x15 -> dec (RL_R LRegister)
          0x16 -> dec RL_ATHL

          0x0f -> dec (RRC_R ARegister)
          0x08 -> dec (RRC_R BRegister)
          0x09 -> dec (RRC_R CRegister)
          0x0a -> dec (RRC_R DRegister)
          0x0b -> dec (RRC_R ERegister)
          0x0c -> dec (RRC_R HRegister)
          0x0d -> dec (RRC_R LRegister)
          0x0e -> dec RRC_ATHL

          0x1f -> dec (RR_R ARegister)
          0x18 -> dec (RR_R BRegister)
          0x19 -> dec (RR_R CRegister)
          0x1a -> dec (RR_R DRegister)
          0x1b -> dec (RR_R ERegister)
          0x1c -> dec (RR_R HRegister)
          0x1d -> dec (RR_R LRegister)
          0x1e -> dec RR_ATHL

          0x27 -> dec (SLA_R ARegister)
          0x20 -> dec (SLA_R BRegister)
          0x21 -> dec (SLA_R CRegister)
          0x22 -> dec (SLA_R DRegister)
          0x23 -> dec (SLA_R ERegister)
          0x24 -> dec (SLA_R HRegister)
          0x25 -> dec (SLA_R LRegister)
          0x26 -> dec SLA_ATHL

          0x2f -> dec (SRA_R ARegister)
          0x28 -> dec (SRA_R BRegister)
          0x29 -> dec (SRA_R CRegister)
          0x2a -> dec (SRA_R DRegister)
          0x2b -> dec (SRA_R ERegister)
          0x2c -> dec (SRA_R HRegister)
          0x2d -> dec (SRA_R LRegister)
          0x2e -> dec SRA_ATHL

          0x3f -> dec (SRL_R ARegister)
          0x38 -> dec (SRL_R BRegister)
          0x39 -> dec (SRL_R CRegister)
          0x3a -> dec (SRL_R DRegister)
          0x3b -> dec (SRL_R ERegister)
          0x3c -> dec (SRL_R HRegister)
          0x3d -> dec (SRL_R LRegister)
          0x3e -> dec SRL_ATHL

          0x47 -> dec (BIT_B_R Bit0 ARegister)
          0x40 -> dec (BIT_B_R Bit0 BRegister)
          0x41 -> dec (BIT_B_R Bit0 CRegister)
          0x42 -> dec (BIT_B_R Bit0 DRegister)
          0x43 -> dec (BIT_B_R Bit0 ERegister)
          0x44 -> dec (BIT_B_R Bit0 HRegister)
          0x45 -> dec (BIT_B_R Bit0 LRegister)
          0x46 -> dec (BIT_B_ATHL Bit0)

          0x48 -> dec (BIT_B_R Bit1 ARegister)
          0x49 -> dec (BIT_B_R Bit1 BRegister)
          0x4a -> dec (BIT_B_R Bit1 CRegister)
          0x4b -> dec (BIT_B_R Bit1 DRegister)
          0x4c -> dec (BIT_B_R Bit1 ERegister)
          0x4d -> dec (BIT_B_R Bit1 HRegister)
          0x4e -> dec (BIT_B_R Bit1 LRegister)
          0x4f -> dec (BIT_B_ATHL Bit1)

          0x57 -> dec (BIT_B_R Bit2 ARegister)
          0x50 -> dec (BIT_B_R Bit2 BRegister)
          0x51 -> dec (BIT_B_R Bit2 CRegister)
          0x52 -> dec (BIT_B_R Bit2 DRegister)
          0x53 -> dec (BIT_B_R Bit2 ERegister)
          0x54 -> dec (BIT_B_R Bit2 HRegister)
          0x55 -> dec (BIT_B_R Bit2 LRegister)
          0x56 -> dec (BIT_B_ATHL Bit2)

          0x58 -> dec (BIT_B_R Bit3 ARegister)
          0x59 -> dec (BIT_B_R Bit3 BRegister)
          0x5a -> dec (BIT_B_R Bit3 CRegister)
          0x5b -> dec (BIT_B_R Bit3 DRegister)
          0x5c -> dec (BIT_B_R Bit3 ERegister)
          0x5d -> dec (BIT_B_R Bit3 HRegister)
          0x5e -> dec (BIT_B_R Bit3 LRegister)
          0x5f -> dec (BIT_B_ATHL Bit3)

          0x67 -> dec (BIT_B_R Bit4 ARegister)
          0x60 -> dec (BIT_B_R Bit4 BRegister)
          0x61 -> dec (BIT_B_R Bit4 CRegister)
          0x62 -> dec (BIT_B_R Bit4 DRegister)
          0x63 -> dec (BIT_B_R Bit4 ERegister)
          0x64 -> dec (BIT_B_R Bit4 HRegister)
          0x65 -> dec (BIT_B_R Bit4 LRegister)
          0x66 -> dec (BIT_B_ATHL Bit4)

          0x68 -> dec (BIT_B_R Bit5 ARegister)
          0x69 -> dec (BIT_B_R Bit5 BRegister)
          0x6a -> dec (BIT_B_R Bit5 CRegister)
          0x6b -> dec (BIT_B_R Bit5 DRegister)
          0x6c -> dec (BIT_B_R Bit5 ERegister)
          0x6d -> dec (BIT_B_R Bit5 HRegister)
          0x6e -> dec (BIT_B_R Bit5 LRegister)
          0x6f -> dec (BIT_B_ATHL Bit5)

          0x77 -> dec (BIT_B_R Bit6 ARegister)
          0x70 -> dec (BIT_B_R Bit6 BRegister)
          0x71 -> dec (BIT_B_R Bit6 CRegister)
          0x72 -> dec (BIT_B_R Bit6 DRegister)
          0x73 -> dec (BIT_B_R Bit6 ERegister)
          0x74 -> dec (BIT_B_R Bit6 HRegister)
          0x75 -> dec (BIT_B_R Bit6 LRegister)
          0x76 -> dec (BIT_B_ATHL Bit6)

          0x78 -> dec (BIT_B_R Bit7 ARegister)
          0x79 -> dec (BIT_B_R Bit7 BRegister)
          0x7a -> dec (BIT_B_R Bit7 CRegister)
          0x7b -> dec (BIT_B_R Bit7 DRegister)
          0x7c -> dec (BIT_B_R Bit7 ERegister)
          0x7d -> dec (BIT_B_R Bit7 HRegister)
          0x7e -> dec (BIT_B_R Bit7 LRegister)
          0x7f -> dec (BIT_B_ATHL Bit7)

          0xc7 -> dec (SET_B_R Bit0 ARegister)
          0xc0 -> dec (SET_B_R Bit0 BRegister)
          0xc1 -> dec (SET_B_R Bit0 CRegister)
          0xc2 -> dec (SET_B_R Bit0 DRegister)
          0xc3 -> dec (SET_B_R Bit0 ERegister)
          0xc4 -> dec (SET_B_R Bit0 HRegister)
          0xc5 -> dec (SET_B_R Bit0 LRegister)
          0xc6 -> dec (SET_B_ATHL Bit0)

          0xc8 -> dec (SET_B_R Bit1 ARegister)
          0xc9 -> dec (SET_B_R Bit1 BRegister)
          0xca -> dec (SET_B_R Bit1 CRegister)
          0xcb -> dec (SET_B_R Bit1 DRegister)
          0xcc -> dec (SET_B_R Bit1 ERegister)
          0xcd -> dec (SET_B_R Bit1 HRegister)
          0xce -> dec (SET_B_R Bit1 LRegister)
          0xcf -> dec (SET_B_ATHL Bit1)

          0xd7 -> dec (SET_B_R Bit2 ARegister)
          0xd0 -> dec (SET_B_R Bit2 BRegister)
          0xd1 -> dec (SET_B_R Bit2 CRegister)
          0xd2 -> dec (SET_B_R Bit2 DRegister)
          0xd3 -> dec (SET_B_R Bit2 ERegister)
          0xd4 -> dec (SET_B_R Bit2 HRegister)
          0xd5 -> dec (SET_B_R Bit2 LRegister)
          0xd6 -> dec (SET_B_ATHL Bit2)

          0xd8 -> dec (SET_B_R Bit3 ARegister)
          0xd9 -> dec (SET_B_R Bit3 BRegister)
          0xda -> dec (SET_B_R Bit3 CRegister)
          0xdb -> dec (SET_B_R Bit3 DRegister)
          0xdc -> dec (SET_B_R Bit3 ERegister)
          0xdd -> dec (SET_B_R Bit3 HRegister)
          0xde -> dec (SET_B_R Bit3 LRegister)
          0xdf -> dec (SET_B_ATHL Bit3)

          0xe7 -> dec (SET_B_R Bit4 ARegister)
          0xe0 -> dec (SET_B_R Bit4 BRegister)
          0xe1 -> dec (SET_B_R Bit4 CRegister)
          0xe2 -> dec (SET_B_R Bit4 DRegister)
          0xe3 -> dec (SET_B_R Bit4 ERegister)
          0xe4 -> dec (SET_B_R Bit4 HRegister)
          0xe5 -> dec (SET_B_R Bit4 LRegister)
          0xe6 -> dec (SET_B_ATHL Bit4)

          0xe8 -> dec (SET_B_R Bit5 ARegister)
          0xe9 -> dec (SET_B_R Bit5 BRegister)
          0xea -> dec (SET_B_R Bit5 CRegister)
          0xeb -> dec (SET_B_R Bit5 DRegister)
          0xec -> dec (SET_B_R Bit5 ERegister)
          0xed -> dec (SET_B_R Bit5 HRegister)
          0xee -> dec (SET_B_R Bit5 LRegister)
          0xef -> dec (SET_B_ATHL Bit5)

          0xf7 -> dec (SET_B_R Bit6 ARegister)
          0xf0 -> dec (SET_B_R Bit6 BRegister)
          0xf1 -> dec (SET_B_R Bit6 CRegister)
          0xf2 -> dec (SET_B_R Bit6 DRegister)
          0xf3 -> dec (SET_B_R Bit6 ERegister)
          0xf4 -> dec (SET_B_R Bit6 HRegister)
          0xf5 -> dec (SET_B_R Bit6 LRegister)
          0xf6 -> dec (SET_B_ATHL Bit6)

          0xf8 -> dec (SET_B_R Bit7 ARegister)
          0xf9 -> dec (SET_B_R Bit7 BRegister)
          0xfa -> dec (SET_B_R Bit7 CRegister)
          0xfb -> dec (SET_B_R Bit7 DRegister)
          0xfc -> dec (SET_B_R Bit7 ERegister)
          0xfd -> dec (SET_B_R Bit7 HRegister)
          0xfe -> dec (SET_B_R Bit7 LRegister)
          0xff -> dec (SET_B_ATHL Bit7)

          0x87 -> dec (RES_B_R Bit0 ARegister)
          0x80 -> dec (RES_B_R Bit0 BRegister)
          0x81 -> dec (RES_B_R Bit0 CRegister)
          0x82 -> dec (RES_B_R Bit0 DRegister)
          0x83 -> dec (RES_B_R Bit0 ERegister)
          0x84 -> dec (RES_B_R Bit0 HRegister)
          0x85 -> dec (RES_B_R Bit0 LRegister)
          0x86 -> dec (RES_B_ATHL Bit0)

          0x88 -> dec (RES_B_R Bit1 ARegister)
          0x89 -> dec (RES_B_R Bit1 BRegister)
          0x8a -> dec (RES_B_R Bit1 CRegister)
          0x8b -> dec (RES_B_R Bit1 DRegister)
          0x8c -> dec (RES_B_R Bit1 ERegister)
          0x8d -> dec (RES_B_R Bit1 HRegister)
          0x8e -> dec (RES_B_R Bit1 LRegister)
          0x8f -> dec (RES_B_ATHL Bit1)

          0x97 -> dec (RES_B_R Bit2 ARegister)
          0x90 -> dec (RES_B_R Bit2 BRegister)
          0x91 -> dec (RES_B_R Bit2 CRegister)
          0x92 -> dec (RES_B_R Bit2 DRegister)
          0x93 -> dec (RES_B_R Bit2 ERegister)
          0x94 -> dec (RES_B_R Bit2 HRegister)
          0x95 -> dec (RES_B_R Bit2 LRegister)
          0x96 -> dec (RES_B_ATHL Bit2)

          0x98 -> dec (RES_B_R Bit3 ARegister)
          0x99 -> dec (RES_B_R Bit3 BRegister)
          0x9a -> dec (RES_B_R Bit3 CRegister)
          0x9b -> dec (RES_B_R Bit3 DRegister)
          0x9c -> dec (RES_B_R Bit3 ERegister)
          0x9d -> dec (RES_B_R Bit3 HRegister)
          0x9e -> dec (RES_B_R Bit3 LRegister)
          0x9f -> dec (RES_B_ATHL Bit3)

          0xa7 -> dec (RES_B_R Bit4 ARegister)
          0xa0 -> dec (RES_B_R Bit4 BRegister)
          0xa1 -> dec (RES_B_R Bit4 CRegister)
          0xa2 -> dec (RES_B_R Bit4 DRegister)
          0xa3 -> dec (RES_B_R Bit4 ERegister)
          0xa4 -> dec (RES_B_R Bit4 HRegister)
          0xa5 -> dec (RES_B_R Bit4 LRegister)
          0xa6 -> dec (RES_B_ATHL Bit4)

          0xa8 -> dec (RES_B_R Bit5 ARegister)
          0xa9 -> dec (RES_B_R Bit5 BRegister)
          0xaa -> dec (RES_B_R Bit5 CRegister)
          0xab -> dec (RES_B_R Bit5 DRegister)
          0xac -> dec (RES_B_R Bit5 ERegister)
          0xad -> dec (RES_B_R Bit5 HRegister)
          0xae -> dec (RES_B_R Bit5 LRegister)
          0xaf -> dec (RES_B_ATHL Bit5)

          0xb7 -> dec (RES_B_R Bit6 ARegister)
          0xb0 -> dec (RES_B_R Bit6 BRegister)
          0xb1 -> dec (RES_B_R Bit6 CRegister)
          0xb2 -> dec (RES_B_R Bit6 DRegister)
          0xb3 -> dec (RES_B_R Bit6 ERegister)
          0xb4 -> dec (RES_B_R Bit6 HRegister)
          0xb5 -> dec (RES_B_R Bit6 LRegister)
          0xb6 -> dec (RES_B_ATHL Bit6)

          0xb8 -> dec (RES_B_R Bit7 ARegister)
          0xb9 -> dec (RES_B_R Bit7 BRegister)
          0xba -> dec (RES_B_R Bit7 CRegister)
          0xbb -> dec (RES_B_R Bit7 DRegister)
          0xbc -> dec (RES_B_R Bit7 ERegister)
          0xbd -> dec (RES_B_R Bit7 HRegister)
          0xbe -> dec (RES_B_R Bit7 LRegister)
          0xbf -> dec (RES_B_ATHL Bit7)

          _ -> return Nothing

      _ -> return Nothing

  where
    dec r = return $ Just r
    decn r = do
      b <- getWord8
      return $ Just $ r b
    decnn r = do
      l <- getWord8
      h <- getWord8
      return $ Just $ r $ makeWord16 l h
