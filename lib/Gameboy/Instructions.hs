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

encodeInstruction (LD_R_R t s) = [code t s]
  where
    code ARegister ARegister = 0x7f
    code ARegister BRegister = 0x78
    code ARegister CRegister = 0x79
    code ARegister DRegister = 0x7a
    code ARegister ERegister = 0x7b
    code ARegister HRegister = 0x7c
    code ARegister LRegister = 0x7d

    code BRegister ARegister = 0x47
    code BRegister BRegister = 0x40
    code BRegister CRegister = 0x41
    code BRegister DRegister = 0x42
    code BRegister ERegister = 0x43
    code BRegister HRegister = 0x44
    code BRegister LRegister = 0x45

    code CRegister ARegister = 0x4f
    code CRegister BRegister = 0x48
    code CRegister CRegister = 0x49
    code CRegister DRegister = 0x4a
    code CRegister ERegister = 0x4b
    code CRegister HRegister = 0x4c
    code CRegister LRegister = 0x4d

    code DRegister ARegister = 0x57
    code DRegister BRegister = 0x50
    code DRegister CRegister = 0x51
    code DRegister DRegister = 0x52
    code DRegister ERegister = 0x53
    code DRegister HRegister = 0x54
    code DRegister LRegister = 0x55

    code ERegister ARegister = 0x5f
    code ERegister BRegister = 0x58
    code ERegister CRegister = 0x59
    code ERegister DRegister = 0x5a
    code ERegister ERegister = 0x5b
    code ERegister HRegister = 0x5c
    code ERegister LRegister = 0x5d

    code HRegister ARegister = 0x67
    code HRegister BRegister = 0x60
    code HRegister CRegister = 0x61
    code HRegister DRegister = 0x62
    code HRegister ERegister = 0x63
    code HRegister HRegister = 0x64
    code HRegister LRegister = 0x65

    code LRegister ARegister = 0x6f
    code LRegister BRegister = 0x68
    code LRegister CRegister = 0x69
    code LRegister DRegister = 0x6a
    code LRegister ERegister = 0x6b
    code LRegister HRegister = 0x6c
    code LRegister LRegister = 0x6d

encodeInstruction (LD_R_N t n) = [code t, n]
  where
    code ARegister = 0x3e
    code BRegister = 0x06
    code CRegister = 0x0e
    code DRegister = 0x16
    code ERegister = 0x1e
    code HRegister = 0x26
    code LRegister = 0x2e

encodeInstruction (LD_R_ATHL t) = [code t]
  where
    code ARegister = 0x7e
    code BRegister = 0x46
    code CRegister = 0x4e
    code DRegister = 0x56
    code ERegister = 0x5e
    code HRegister = 0x66
    code LRegister = 0x6e

encodeInstruction (LD_ATHL_R s) = [code s]
  where
    code ARegister = 0x77
    code BRegister = 0x70
    code CRegister = 0x71
    code DRegister = 0x72
    code ERegister = 0x73
    code HRegister = 0x74
    code LRegister = 0x75

encodeInstruction (LD_ATHL_N n) = [0x36, n]

encodeInstruction NOP = [0x0]
encodeInstruction STOP = [0x10, 0x00]

encodeInstruction _ = error "instruction encode unimplemented!"

decodeInstruction :: (Monad m) => m Word8 -> m (Maybe Instruction)
decodeInstruction getWord8 = do
  w <- getWord8
  case w of
    0x7f -> Just <$> return (LD_R_R ARegister ARegister)
    0x78 -> Just <$> return (LD_R_R ARegister BRegister)
    0x79 -> Just <$> return (LD_R_R ARegister CRegister)
    0x7a -> Just <$> return (LD_R_R ARegister DRegister)
    0x7b -> Just <$> return (LD_R_R ARegister ERegister)
    0x7c -> Just <$> return (LD_R_R ARegister HRegister)
    0x7d -> Just <$> return (LD_R_R ARegister LRegister)

    0x47 -> Just <$> return (LD_R_R BRegister ARegister)
    0x40 -> Just <$> return (LD_R_R BRegister BRegister)
    0x41 -> Just <$> return (LD_R_R BRegister CRegister)
    0x42 -> Just <$> return (LD_R_R BRegister DRegister)
    0x43 -> Just <$> return (LD_R_R BRegister ERegister)
    0x44 -> Just <$> return (LD_R_R BRegister HRegister)
    0x45 -> Just <$> return (LD_R_R BRegister LRegister)

    0x4f -> Just <$> return (LD_R_R CRegister ARegister)
    0x48 -> Just <$> return (LD_R_R CRegister BRegister)
    0x49 -> Just <$> return (LD_R_R CRegister CRegister)
    0x4a -> Just <$> return (LD_R_R CRegister DRegister)
    0x4b -> Just <$> return (LD_R_R CRegister ERegister)
    0x4c -> Just <$> return (LD_R_R CRegister HRegister)
    0x4d -> Just <$> return (LD_R_R CRegister LRegister)

    0x57 -> Just <$> return (LD_R_R DRegister ARegister)
    0x50 -> Just <$> return (LD_R_R DRegister BRegister)
    0x51 -> Just <$> return (LD_R_R DRegister CRegister)
    0x52 -> Just <$> return (LD_R_R DRegister DRegister)
    0x53 -> Just <$> return (LD_R_R DRegister ERegister)
    0x54 -> Just <$> return (LD_R_R DRegister HRegister)
    0x55 -> Just <$> return (LD_R_R DRegister LRegister)

    0x5f -> Just <$> return (LD_R_R ERegister ARegister)
    0x58 -> Just <$> return (LD_R_R ERegister BRegister)
    0x59 -> Just <$> return (LD_R_R ERegister CRegister)
    0x5a -> Just <$> return (LD_R_R ERegister DRegister)
    0x5b -> Just <$> return (LD_R_R ERegister ERegister)
    0x5c -> Just <$> return (LD_R_R ERegister HRegister)
    0x5d -> Just <$> return (LD_R_R ERegister LRegister)

    0x67 -> Just <$> return (LD_R_R HRegister ARegister)
    0x60 -> Just <$> return (LD_R_R HRegister BRegister)
    0x61 -> Just <$> return (LD_R_R HRegister CRegister)
    0x62 -> Just <$> return (LD_R_R HRegister DRegister)
    0x63 -> Just <$> return (LD_R_R HRegister ERegister)
    0x64 -> Just <$> return (LD_R_R HRegister HRegister)
    0x65 -> Just <$> return (LD_R_R HRegister LRegister)

    0x6f -> Just <$> return (LD_R_R LRegister ARegister)
    0x68 -> Just <$> return (LD_R_R LRegister BRegister)
    0x69 -> Just <$> return (LD_R_R LRegister CRegister)
    0x6a -> Just <$> return (LD_R_R LRegister DRegister)
    0x6b -> Just <$> return (LD_R_R LRegister ERegister)
    0x6c -> Just <$> return (LD_R_R LRegister HRegister)
    0x6d -> Just <$> return (LD_R_R LRegister LRegister)

    0x3e -> Just <$> LD_R_N ARegister <$> getWord8
    0x06 -> Just <$> LD_R_N BRegister <$> getWord8
    0x0e -> Just <$> LD_R_N CRegister <$> getWord8
    0x16 -> Just <$> LD_R_N DRegister <$> getWord8
    0x1e -> Just <$> LD_R_N ERegister <$> getWord8
    0x26 -> Just <$> LD_R_N HRegister <$> getWord8
    0x2e -> Just <$> LD_R_N LRegister <$> getWord8

    0x7e -> Just <$> return (LD_R_ATHL ARegister)
    0x46 -> Just <$> return (LD_R_ATHL BRegister)
    0x4e -> Just <$> return (LD_R_ATHL CRegister)
    0x56 -> Just <$> return (LD_R_ATHL DRegister)
    0x5e -> Just <$> return (LD_R_ATHL ERegister)
    0x66 -> Just <$> return (LD_R_ATHL HRegister)
    0x6e -> Just <$> return (LD_R_ATHL LRegister)

    0x77 -> Just <$> return (LD_ATHL_R ARegister)
    0x70 -> Just <$> return (LD_ATHL_R BRegister)
    0x71 -> Just <$> return (LD_ATHL_R CRegister)
    0x72 -> Just <$> return (LD_ATHL_R DRegister)
    0x73 -> Just <$> return (LD_ATHL_R ERegister)
    0x74 -> Just <$> return (LD_ATHL_R HRegister)
    0x75 -> Just <$> return (LD_ATHL_R LRegister)

    0x36 -> Just <$> LD_ATHL_N <$> getWord8

    0x00 -> return $ Just NOP
    0x10 -> do
      w2 <- getWord8 
      case w2 of
        0x00 -> return $ Just STOP
        _ -> return Nothing

    _ -> return Nothing
