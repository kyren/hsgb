module Gameboy.Assembler (
  parseInstructions,
  encodeInstructions,
  assemble
) where

import Prelude hiding (and, or)
import Data.Maybe
import Data.Char
import Data.Word
import Control.Monad
import Text.Parsec
import qualified Data.Vector.Unboxed as VU
import Gameboy.Instructions

data Register16
  = AFRegister16
  | BCRegister16
  | DERegister16
  | HLRegister16
  | SPRegister16
  deriving (Show)

data Address
  = AtHL
  | AtC
  | AtBC
  | AtDE
  | AtNN Word16
  | AtN Word8
  deriving (Show)

data Argument 
  = RegisterArg Register
  | Register16Arg Register16
  | AddressArg Address
  | I8Arg Word8
  | I16Arg Word16
  deriving (Show)

hexToInt :: String -> Integer
hexToInt = foldl (\a i -> a * 16 + fromIntegral (digitToInt i)) 0

spaces1 :: Parsec String st String
spaces1 = many1 space

immediate8 :: Parsec String st Word8
immediate8 = do
  _ <- char '$'
  digits <- count 2 hexDigit
  return $ fromIntegral $ hexToInt digits

immediate16 :: Parsec String st Word16
immediate16 = do
  _ <- char '$'
  digits <- count 4 hexDigit
  return $ fromIntegral $ hexToInt digits

registerArg :: Parsec String st Argument
registerArg = a <|> b <|> c <|> d <|> e <|> h <|> l
  where
    a = char 'A' >> return (RegisterArg ARegister)
    b = char 'B' >> return (RegisterArg BRegister)
    c = char 'C' >> return (RegisterArg CRegister)
    d = char 'D' >> return (RegisterArg DRegister)
    e = char 'E' >> return (RegisterArg ERegister)
    h = char 'H' >> return (RegisterArg HRegister)
    l = char 'L' >> return (RegisterArg LRegister)

register16Arg :: Parsec String st Argument
register16Arg = af <|> bc <|> de <|> hl <|> sp
  where
    af = string "AF" >> return (Register16Arg AFRegister16)
    bc = string "BC" >> return (Register16Arg BCRegister16)
    de = string "DE" >> return (Register16Arg DERegister16)
    hl = string "HL" >> return (Register16Arg HLRegister16)
    sp = string "SP" >> return (Register16Arg SPRegister16)

addressArg :: Parsec String st Argument
addressArg = do
    _ <- char '('
    r <- hl <|> c <|> bc <|> de <|> try nn <|> n
    _ <- char ')'
    return r
  where
    hl = string "HL" >> return (AddressArg AtHL)
    c = string "C" >> return (AddressArg AtC)
    bc = string "BC" >> return (AddressArg AtBC)
    de = string "DE" >> return (AddressArg AtDE)
    nn = AddressArg <$> AtNN <$> immediate16
    n = AddressArg <$> AtN <$> immediate8

regularArg :: Parsec String st Argument
regularArg
  = try addressArg
  <|> try (I16Arg <$> immediate16)
  <|> try (I8Arg <$> immediate8)
  <|> try register16Arg
  <|> registerArg

bitArg :: Parsec String st Bit
bitArg = b0 <|> b1 <|> b2 <|> b3 <|> b4 <|> b5 <|> b6 <|> b7
  where
    b0 = char '0' >> return Bit0
    b1 = char '1' >> return Bit1
    b2 = char '2' >> return Bit2
    b3 = char '3' >> return Bit3
    b4 = char '4' >> return Bit4
    b5 = char '5' >> return Bit5
    b6 = char '6' >> return Bit6
    b7 = char '7' >> return Bit7

condArg :: Parsec String st Cond
condArg = z <|> c <|> try nz <|> nc
  where
    z = char 'Z' >> return Zero
    c = char 'C' >> return Carry
    nz = string "NZ" >> return NZero
    nc = string "NC" >> return NCarry

instruction1Arg :: String -> Parsec String st a1 -> Parsec String st a1
instruction1Arg inst arg1 = do
  _ <- string inst >> spaces1
  arg1

instruction2Arg :: String -> Parsec String st a1 -> Parsec String st a2 -> Parsec String st (a1, a2)
instruction2Arg inst arg1 arg2 = do
  _ <- string inst >> spaces1
  a1 <- arg1
  _ <- spaces >> char ',' >> spaces
  a2 <- arg2
  return (a1, a2)

load8 :: Parsec String st Instruction
load8 = do
    (t, s) <- instruction2Arg "LD" regularArg regularArg
    encode t s
  where
    encode (RegisterArg t) (RegisterArg s) = return $ LD_R_R t s
    encode (RegisterArg t) (AddressArg AtHL) = return $ LD_R_ATHL t
    encode (RegisterArg t) (I8Arg n) = return $ LD_R_N t n
    encode (AddressArg AtHL) (RegisterArg s) = return $ LD_ATHL_R s
    encode (AddressArg AtHL) (I8Arg n) = return $ LD_ATHL_N n
    encode (RegisterArg ARegister) (AddressArg AtC) = return LD_A_ATC
    encode (RegisterArg ARegister) (AddressArg AtBC) = return LD_A_ATBC
    encode (RegisterArg ARegister) (AddressArg AtDE) = return LD_A_ATDE
    encode (RegisterArg ARegister) (AddressArg (AtNN nn)) = return $ LD_A_ATNN nn
    encode (AddressArg AtC) (RegisterArg ARegister) = return LD_ATC_A
    encode (AddressArg AtBC) (RegisterArg ARegister) = return LD_ATBC_A
    encode (AddressArg AtDE) (RegisterArg ARegister) = return LD_ATDE_A
    encode (AddressArg (AtNN nn)) (RegisterArg ARegister) = return $ LD_ATNN_A nn
    encode _ _ = fail "Invalid LD instruction"

load8dec :: Parsec String st Instruction
load8dec = do
    (t, s) <- instruction2Arg "LDD" regularArg regularArg
    encode t s
  where
    encode (RegisterArg ARegister) (AddressArg AtHL) = return LDD_A_ATHL
    encode (AddressArg AtHL) (RegisterArg ARegister) = return LDD_ATHL_A
    encode _ _ = fail "Invalid LDD instruction"

load8inc :: Parsec String st Instruction
load8inc = do
    (t, s) <- instruction2Arg "LDI" regularArg regularArg
    encode t s
  where
    encode (RegisterArg ARegister) (AddressArg AtHL) = return LDI_A_ATHL
    encode (AddressArg AtHL) (RegisterArg ARegister) = return LDI_ATHL_A
    encode _ _ = fail "Invalid LDI instruction"

loadh :: Parsec String st Instruction
loadh = do
    (t, s) <- instruction2Arg "LDH" regularArg regularArg
    encode t s
  where
    encode (RegisterArg ARegister) (AddressArg (AtN n)) = return $ LDH_A_ATN n
    encode (AddressArg (AtN n)) (RegisterArg ARegister) = return $ LDH_ATN_A n
    encode _ _ = fail "Invalid LDH instruction"

load16 :: Parsec String st Instruction
load16 = do
    (t, s) <- instruction2Arg "LD" regularArg regularArg
    encode t s
  where
    encode (Register16Arg BCRegister16) (I16Arg nn) = return $ LD_BC_NN nn
    encode (Register16Arg DERegister16) (I16Arg nn) = return $ LD_DE_NN nn
    encode (Register16Arg HLRegister16) (I16Arg nn) = return $ LD_HL_NN nn
    encode (Register16Arg SPRegister16) (I16Arg nn) = return $ LD_SP_NN nn
    encode (Register16Arg SPRegister16) (Register16Arg HLRegister16) = return LD_SP_HL
    encode (AddressArg (AtNN nn)) (Register16Arg SPRegister16) = return $ LD_ATNN_SP nn
    encode _ _ = fail "Invalid LD instruction"

loadhl :: Parsec String st Instruction
loadhl = do
    (t, s) <- instruction2Arg "LDHL" regularArg regularArg
    encode t s
  where
    encode (Register16Arg SPRegister16) (I8Arg n) = return $ LDHL_SP_N n
    encode _ _ = fail "Invalid LDHL instruction"

push :: Parsec String st Instruction
push = instruction1Arg "PUSH" regularArg >>= encode
  where
    encode (Register16Arg AFRegister16) = return PUSH_AF
    encode (Register16Arg BCRegister16) = return PUSH_BC
    encode (Register16Arg DERegister16) = return PUSH_DE
    encode (Register16Arg HLRegister16) = return PUSH_HL
    encode _ = fail "Invalid PUSH instruction"

pop :: Parsec String st Instruction
pop = instruction1Arg "POP" regularArg >>= encode
  where
    encode (Register16Arg AFRegister16) = return POP_AF
    encode (Register16Arg BCRegister16) = return POP_BC
    encode (Register16Arg DERegister16) = return POP_DE
    encode (Register16Arg HLRegister16) = return POP_HL
    encode _ = fail "Invalid POP instruction"

add :: Parsec String st Instruction
add = do
    (t, s) <- instruction2Arg "ADD" regularArg regularArg
    encode t s
  where
    encode (RegisterArg ARegister) (RegisterArg r) = return $ ADD_A_R r
    encode (RegisterArg ARegister) (I8Arg n) = return $ ADD_A_N n
    encode (RegisterArg ARegister) (AddressArg AtHL) = return ADD_A_ATHL
    encode (Register16Arg HLRegister16) (Register16Arg BCRegister16) = return ADD_HL_BC
    encode (Register16Arg HLRegister16) (Register16Arg DERegister16) = return ADD_HL_DE
    encode (Register16Arg HLRegister16) (Register16Arg HLRegister16) = return ADD_HL_HL
    encode (Register16Arg HLRegister16) (Register16Arg SPRegister16) = return ADD_HL_SP
    encode (Register16Arg SPRegister16) (I8Arg n) = return $ ADD_SP_N n
    encode _ _ = fail "Invalid ADD instruction"

adc :: Parsec String st Instruction
adc = do
    (t, s) <- instruction2Arg "ADC" regularArg regularArg
    encode t s
  where
    encode (RegisterArg ARegister) (RegisterArg r) = return $ ADC_A_R r
    encode (RegisterArg ARegister) (I8Arg n) = return $ ADC_A_N n
    encode (RegisterArg ARegister) (AddressArg AtHL) = return ADC_A_ATHL
    encode _ _ = fail "Invalid ADC instruction"

sub :: Parsec String st Instruction
sub = instruction1Arg "SUB" regularArg >>= encode
  where
    encode (RegisterArg r) = return $ SUB_R r
    encode (I8Arg n) = return $ SUB_N n
    encode (AddressArg AtHL) = return SUB_ATHL
    encode _ = fail "Invalid SUB instruction"

sbc :: Parsec String st Instruction
sbc = do
    (t, s) <- instruction2Arg "SBC" regularArg regularArg
    encode t s
  where
    encode (RegisterArg ARegister) (RegisterArg r) = return $ SBC_A_R r
    encode (RegisterArg ARegister) (I8Arg n) = return $ SBC_A_N n
    encode (RegisterArg ARegister) (AddressArg AtHL) = return SBC_A_ATHL
    encode _ _ = fail "Invalid SBC instruction"

and :: Parsec String st Instruction
and = instruction1Arg "AND" regularArg >>= encode
  where
    encode (RegisterArg r) = return $ AND_R r
    encode (I8Arg n) = return $ AND_N n
    encode (AddressArg AtHL) = return AND_ATHL
    encode _ = fail "Invalid AND instruction"

or :: Parsec String st Instruction
or = instruction1Arg "OR" regularArg >>= encode
  where
    encode (RegisterArg r) = return $ OR_R r
    encode (I8Arg n) = return $ OR_N n
    encode (AddressArg AtHL) = return OR_ATHL
    encode _ = fail "Invalid OR instruction"

xor :: Parsec String st Instruction
xor = instruction1Arg "XOR" regularArg >>= encode
  where
    encode (RegisterArg r) = return $ XOR_R r
    encode (I8Arg n) = return $ XOR_N n
    encode (AddressArg AtHL) = return XOR_ATHL
    encode _ = fail "Invalid XOR instruction"

cp :: Parsec String st Instruction
cp = instruction1Arg "CP" regularArg >>= encode
  where
    encode (RegisterArg r) = return $ CP_R r
    encode (I8Arg n) = return $ CP_N n
    encode (AddressArg AtHL) = return CP_ATHL
    encode _ = fail "Invalid CP instruction"

inc :: Parsec String st Instruction
inc = instruction1Arg "INC" regularArg >>= encode
  where
    encode (RegisterArg r) = return $ INC_R r
    encode (AddressArg AtHL) = return INC_ATHL
    encode (Register16Arg BCRegister16) = return INC_BC
    encode (Register16Arg DERegister16) = return INC_DE
    encode (Register16Arg HLRegister16) = return INC_HL
    encode (Register16Arg SPRegister16) = return INC_SP
    encode _ = fail "Invalid INC instruction"

dec :: Parsec String st Instruction
dec = instruction1Arg "DEC" regularArg >>= encode
  where
    encode (RegisterArg r) = return $ DEC_R r
    encode (AddressArg AtHL) = return DEC_ATHL
    encode (Register16Arg BCRegister16) = return DEC_BC
    encode (Register16Arg DERegister16) = return DEC_DE
    encode (Register16Arg HLRegister16) = return DEC_HL
    encode (Register16Arg SPRegister16) = return DEC_SP
    encode _ = fail "Invalid DEC instruction"

swap :: Parsec String st Instruction
swap = instruction1Arg "SWAP" regularArg >>= encode
  where
    encode (RegisterArg r) = return $ SWAP_R r
    encode (AddressArg AtHL) = return SWAP_ATHL
    encode _ = fail "Invalid SWAP instruction"

daa :: Parsec String st Instruction
daa = string "DAA" >> return DAA

cpl :: Parsec String st Instruction
cpl = string "CPL" >> return CPL

ccf :: Parsec String st Instruction
ccf = string "CCF" >> return CCF

scf :: Parsec String st Instruction
scf = string "SCF" >> return SCF

nop :: Parsec String st Instruction
nop = string "NOP" >> return NOP

halt :: Parsec String st Instruction
halt = string "HALT" >> return HALT

stop :: Parsec String st Instruction
stop = string "STOP" >> return STOP

di :: Parsec String st Instruction
di = string "DI" >> return DI

ei :: Parsec String st Instruction
ei = string "EI" >> return EI

rlca :: Parsec String st Instruction
rlca = string "RLCA" >> return RLCA

rla :: Parsec String st Instruction
rla = string "RLA" >> return RLA

rrca :: Parsec String st Instruction
rrca = string "RRCA" >> return RRCA

rra :: Parsec String st Instruction
rra = string "RRA" >> return RRA

rlc :: Parsec String st Instruction
rlc = instruction1Arg "RLC" regularArg >>= encode
  where
    encode (RegisterArg r) = return $ RLC_R r
    encode (AddressArg AtHL) = return RLC_ATHL
    encode _ = fail "Invalid RLC instruction"

rl :: Parsec String st Instruction
rl = instruction1Arg "RL" regularArg >>= encode
  where
    encode (RegisterArg r) = return $ RL_R r
    encode (AddressArg AtHL) = return RL_ATHL
    encode _ = fail "Invalid RL instruction"

rrc :: Parsec String st Instruction
rrc = instruction1Arg "RRC" regularArg >>= encode
  where
    encode (RegisterArg r) = return $ RRC_R r
    encode (AddressArg AtHL) = return RRC_ATHL
    encode _ = fail "Invalid RRC instruction"

rr :: Parsec String st Instruction
rr = instruction1Arg "RR" regularArg >>= encode
  where
    encode (RegisterArg r) = return $ RR_R r
    encode (AddressArg AtHL) = return RR_ATHL
    encode _ = fail "Invalid RR instruction"

sla :: Parsec String st Instruction
sla = instruction1Arg "SLA" regularArg >>= encode
  where
    encode (RegisterArg r) = return $ SLA_R r
    encode (AddressArg AtHL) = return SLA_ATHL
    encode _ = fail "Invalid SLA instruction"

sra :: Parsec String st Instruction
sra = instruction1Arg "SRA" regularArg >>= encode
  where
    encode (RegisterArg r) = return $ SRA_R r
    encode (AddressArg AtHL) = return SRA_ATHL
    encode _ = fail "Invalid SRA instruction"

srl :: Parsec String st Instruction
srl = instruction1Arg "SRL" regularArg >>= encode
  where
    encode (RegisterArg r) = return $ SRL_R r
    encode (AddressArg AtHL) = return SRL_ATHL
    encode _ = fail "Invalid SRL instruction"

bit :: Parsec String st Instruction
bit = do
    (b, t) <- instruction2Arg "BIT" bitArg regularArg
    encode b t
  where
    encode b (RegisterArg r) = return $ BIT_B_R b r
    encode b (AddressArg AtHL) = return $ BIT_B_ATHL b
    encode _ _ = fail "Invalid BIT instruction"

set :: Parsec String st Instruction
set = do
    (b, t) <- instruction2Arg "SET" bitArg regularArg
    encode b t
  where
    encode b (RegisterArg r) = return $ SET_B_R b r
    encode b (AddressArg AtHL) = return $ SET_B_ATHL b
    encode _ _ = fail "Invalid SET instruction"

res :: Parsec String st Instruction
res = do
    (b, t) <- instruction2Arg "RES" bitArg regularArg
    encode b t
  where
    encode b (RegisterArg r) = return $ RES_B_R b r
    encode b (AddressArg AtHL) = return $ RES_B_ATHL b
    encode _ _ = fail "Invalid RES instruction"

jp :: Parsec String st Instruction
jp = try jp2 <|> jp1
  where
    jp2 = do
        (c, a) <- instruction2Arg "JP" condArg regularArg
        enc2 c a
    jp1 = instruction1Arg "JP" regularArg >>= enc1
    enc1 (I16Arg nn) = return $ JP_NN nn
    enc1 (AddressArg AtHL) = return JP_ATHL
    enc1 _ = fail "Invalid JP instruction"
    enc2 c (I16Arg nn) = return $ JP_C_NN c nn
    enc2 _ _ = fail "Invalid JP instruction"

jr :: Parsec String st Instruction
jr = try jr2 <|> jr1
  where
    jr2 = do
        (c, a) <- instruction2Arg "JR" condArg regularArg
        enc2 c a
    jr1 = instruction1Arg "JR" regularArg >>= enc1
    enc1 (I8Arg n) = return $ JR_N n
    enc1 _ = fail "Invalid JR instruction"
    enc2 c (I8Arg n) = return $ JR_C_N c n
    enc2 _ _ = fail "Invalid JR instruction"

call :: Parsec String st Instruction
call = try call2 <|> call1
  where
    call2 = do
        (c, a) <- instruction2Arg "CALL" condArg immediate16
        return $ CALL_C_NN c a
    call1 = CALL_NN <$> instruction1Arg "CALL" immediate16

rst :: Parsec String st Instruction
rst = instruction1Arg "RST" immediate8 >>= encode
  where
    encode 0x00 = return $ RST_RA Reset00
    encode 0x08 = return $ RST_RA Reset08
    encode 0x10 = return $ RST_RA Reset10
    encode 0x18 = return $ RST_RA Reset18
    encode 0x20 = return $ RST_RA Reset20
    encode 0x28 = return $ RST_RA Reset28
    encode 0x30 = return $ RST_RA Reset30
    encode 0x38 = return $ RST_RA Reset38
    encode _ = fail "Invalid RST address"

ret :: Parsec String st Instruction
ret = try ret1 <|> ret0
  where
    ret1 = RET_C <$> instruction1Arg "RET" condArg
    ret0 = string "RET" >> return RET

reti :: Parsec String st Instruction
reti = string "RETI" >> return RETI

instruction :: Parsec String st Instruction
instruction
  = try load8
  <|> try load8dec
  <|> try load8inc
  <|> try loadhl
  <|> try load16
  <|> try loadh
  <|> try push
  <|> try pop
  <|> try add
  <|> try adc
  <|> try sub
  <|> try sbc
  <|> try and
  <|> try or
  <|> try xor
  <|> try cp
  <|> try inc
  <|> try dec
  <|> try swap
  <|> try daa
  <|> try cpl
  <|> try ccf
  <|> try scf
  <|> try nop
  <|> try halt
  <|> try stop
  <|> try di
  <|> try ei
  <|> try rlca
  <|> try rla
  <|> try rrca
  <|> try rra
  <|> try rlc
  <|> try rl
  <|> try rrc
  <|> try rr
  <|> try sla
  <|> try sra
  <|> try srl
  <|> try bit
  <|> try set
  <|> try res
  <|> try jp
  <|> try jr
  <|> try call
  <|> try rst
  <|> try reti
  <|> ret

comment :: Parsec String st ()
comment = do
  _ <- char ';'
  _ <- many (noneOf "\r\n")
  return ()

instructionLine :: Parsec String st (Maybe Instruction)
instructionLine = do
  skipMany (oneOf " \t")
  mi <- optionMaybe instruction
  skipMany (oneOf " \t")
  optional comment
  return mi

instructions :: Parsec String st [Instruction]
instructions = do
  r <- liftM catMaybes $ instructionLine `sepBy1` endOfLine
  optional endOfLine
  eof
  return r

parseInstructions :: String -> Either String [Instruction]
parseInstructions input = case parse instructions "" input of
  Left err -> Left (show err)
  Right result -> Right result

encodeInstructions :: [Instruction] -> VU.Vector Word8
encodeInstructions insts = VU.fromList <$> concat $ map encodeInstruction insts

assemble :: String -> Either String (VU.Vector Word8)
assemble text = do
  inst <- parseInstructions text
  return $ encodeInstructions inst
