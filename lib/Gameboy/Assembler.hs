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

register :: Parsec String st Register
register = a <|> b <|> c <|> d <|> e <|> h <|> l
  where
    a = char 'A' >> return ARegister
    b = char 'B' >> return BRegister
    c = char 'C' >> return CRegister
    d = char 'D' >> return DRegister
    e = char 'E' >> return ERegister
    h = char 'H' >> return HRegister
    l = char 'L' >> return LRegister

register16 :: Parsec String st Register16
register16 = af <|> bc <|> de <|> hl <|> sp
  where
    af = string "AF" >> return AFRegister16
    bc = string "BC" >> return BCRegister16
    de = string "DE" >> return DERegister16
    hl = string "HL" >> return HLRegister16
    sp = string "SP" >> return SPRegister16

addr :: Parsec String st Address
addr = do
    _ <- char '('
    res <- hl <|> c <|> bc <|> de <|> try nn <|> n
    _ <- char ')'
    return res
  where
    hl = string "HL" >> return AtHL
    c = string "C" >> return AtC
    bc = string "BC" >> return AtBC
    de = string "DE" >> return AtDE
    nn = AtNN <$> immediate16
    n = AtN <$> immediate8

argument :: Parsec String st Argument
argument
  = try (AddressArg <$> addr)
  <|> try (I16Arg <$> immediate16)
  <|> try (I8Arg <$> immediate8)
  <|> try (Register16Arg <$> register16)
  <|> RegisterArg <$> register

instructionArg1 :: String -> Parsec String st Argument
instructionArg1 inst = do
  _ <- string inst >> spaces1
  argument

instructionArg2 :: String -> Parsec String st (Argument, Argument)
instructionArg2 inst = do
  _ <- string inst >> spaces1
  a1 <- argument
  _ <- spaces >> char ',' >> spaces
  a2 <- argument
  return (a1, a2)

load8 :: Parsec String st Instruction
load8 = do
    (t, s) <- instructionArg2 "LD"
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
    (t, s) <- instructionArg2 "LDD"
    encode t s
  where
    encode (RegisterArg ARegister) (AddressArg AtHL) = return LDD_A_ATHL
    encode (AddressArg AtHL) (RegisterArg ARegister) = return LDD_ATHL_A
    encode _ _ = fail "Invalid LDD instruction"

load8inc :: Parsec String st Instruction
load8inc = do
    (t, s) <- instructionArg2 "LDI"
    encode t s
  where
    encode (RegisterArg ARegister) (AddressArg AtHL) = return LDI_A_ATHL
    encode (AddressArg AtHL) (RegisterArg ARegister) = return LDI_ATHL_A
    encode _ _ = fail "Invalid LDI instruction"

loadh :: Parsec String st Instruction
loadh = do
    (t, s) <- instructionArg2 "LDH"
    encode t s
  where
    encode (RegisterArg ARegister) (AddressArg (AtN n)) = return $ LDH_A_ATN n
    encode (AddressArg (AtN n)) (RegisterArg ARegister) = return $ LDH_ATN_A n
    encode _ _ = fail "Invalid LDH instruction"

load16 :: Parsec String st Instruction
load16 = do
    (t, s) <- instructionArg2 "LD"
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
    (t, s) <- instructionArg2 "LDHL"
    encode t s
  where
    encode (Register16Arg SPRegister16) (I8Arg n) = return $ LDHL_SP_N n
    encode _ _ = fail "Invalid LDHL instruction"

push :: Parsec String st Instruction
push = instructionArg1 "PUSH" >>= encode
  where
    encode (Register16Arg AFRegister16) = return PUSH_AF
    encode (Register16Arg BCRegister16) = return PUSH_BC
    encode (Register16Arg DERegister16) = return PUSH_DE
    encode (Register16Arg HLRegister16) = return PUSH_HL
    encode _ = fail "Invalid PUSH instruction"

pop :: Parsec String st Instruction
pop = instructionArg1 "POP" >>= encode
  where
    encode (Register16Arg AFRegister16) = return POP_AF
    encode (Register16Arg BCRegister16) = return POP_BC
    encode (Register16Arg DERegister16) = return POP_DE
    encode (Register16Arg HLRegister16) = return POP_HL
    encode _ = fail "Invalid POP instruction"

add :: Parsec String st Instruction
add = do
    (t, s) <- instructionArg2 "ADD"
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
    (t, s) <- instructionArg2 "ADC"
    encode t s
  where
    encode (RegisterArg ARegister) (RegisterArg r) = return $ ADC_A_R r
    encode (RegisterArg ARegister) (I8Arg n) = return $ ADC_A_N n
    encode (RegisterArg ARegister) (AddressArg AtHL) = return ADC_A_ATHL
    encode _ _ = fail "Invalid ADC instruction"

sub :: Parsec String st Instruction
sub = instructionArg1 "SUB" >>= encode
  where
    encode (RegisterArg r) = return $ SUB_R r
    encode (I8Arg n) = return $ SUB_N n
    encode (AddressArg AtHL) = return SUB_ATHL
    encode _ = fail "Invalid SUB instruction"

sbc :: Parsec String st Instruction
sbc = do
    (t, s) <- instructionArg2 "SBC"
    encode t s
  where
    encode (RegisterArg ARegister) (RegisterArg r) = return $ SBC_A_R r
    encode (RegisterArg ARegister) (I8Arg n) = return $ SBC_A_N n
    encode (RegisterArg ARegister) (AddressArg AtHL) = return SBC_A_ATHL
    encode _ _ = fail "Invalid SBC instruction"

and :: Parsec String st Instruction
and = instructionArg1 "AND" >>= encode
  where
    encode (RegisterArg r) = return $ AND_R r
    encode (I8Arg n) = return $ AND_N n
    encode (AddressArg AtHL) = return AND_ATHL
    encode _ = fail "Invalid AND instruction"

or :: Parsec String st Instruction
or = instructionArg1 "OR" >>= encode
  where
    encode (RegisterArg r) = return $ OR_R r
    encode (I8Arg n) = return $ OR_N n
    encode (AddressArg AtHL) = return OR_ATHL
    encode _ = fail "Invalid OR instruction"

xor :: Parsec String st Instruction
xor = instructionArg1 "XOR" >>= encode
  where
    encode (RegisterArg r) = return $ XOR_R r
    encode (I8Arg n) = return $ XOR_N n
    encode (AddressArg AtHL) = return XOR_ATHL
    encode _ = fail "Invalid XOR instruction"

cp :: Parsec String st Instruction
cp = instructionArg1 "CP" >>= encode
  where
    encode (RegisterArg r) = return $ CP_R r
    encode (I8Arg n) = return $ CP_N n
    encode (AddressArg AtHL) = return CP_ATHL
    encode _ = fail "Invalid CP instruction"

inc :: Parsec String st Instruction
inc = instructionArg1 "INC" >>= encode
  where
    encode (RegisterArg r) = return $ INC_R r
    encode (AddressArg AtHL) = return INC_ATHL
    encode (Register16Arg BCRegister16) = return INC_BC
    encode (Register16Arg DERegister16) = return INC_DE
    encode (Register16Arg HLRegister16) = return INC_HL
    encode (Register16Arg SPRegister16) = return INC_SP
    encode _ = fail "Invalid INC instruction"

dec :: Parsec String st Instruction
dec = instructionArg1 "DEC" >>= encode
  where
    encode (RegisterArg r) = return $ DEC_R r
    encode (AddressArg AtHL) = return DEC_ATHL
    encode (Register16Arg BCRegister16) = return DEC_BC
    encode (Register16Arg DERegister16) = return DEC_DE
    encode (Register16Arg HLRegister16) = return DEC_HL
    encode (Register16Arg SPRegister16) = return DEC_SP
    encode _ = fail "Invalid DEC instruction"

swap :: Parsec String st Instruction
swap = instructionArg1 "SWAP" >>= encode
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
  <|> rra

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
  res <- liftM catMaybes $ instructionLine `sepBy1` endOfLine
  optional endOfLine
  eof
  return res

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
