module Gameboy.Assembler (
  parseInstructions,
  encodeInstructions,
  assemble
) where

import Data.Maybe
import Data.Char
import Data.Word
import Control.Monad
import Text.Parsec
import qualified Data.Vector.Unboxed as VU
import Gameboy.Instructions

data RegisterPair
  = AFRegPair
  | BCRegPair
  | DERegPair
  | HLRegPair
  deriving (Show)

data Addr
  = AtHL
  | AtC
  | AtBC
  | AtDE
  | AtNN Word16
  | AtN Word8
  deriving (Show)

data Argument 
  = RegArg Register
  | RegPairArg RegisterPair
  | AddrArg Addr
  | I8Arg Word8
  | I16Arg Word16
  | SPArg
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

registerPair :: Parsec String st RegisterPair
registerPair = af <|> bc <|> de <|> hl
  where
    af = string "AF" >> return AFRegPair
    bc = string "BC" >> return BCRegPair
    de = string "DE" >> return DERegPair
    hl = string "HL" >> return HLRegPair

addr :: Parsec String st Addr
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

sp :: Parsec String st Argument
sp = string "SP" >> return SPArg

argument :: Parsec String st Argument
argument
  = try (AddrArg <$> addr)
  <|> try (I16Arg <$> immediate16)
  <|> try (I8Arg <$> immediate8)
  <|> try (RegPairArg <$> registerPair)
  <|> RegArg <$> register
  <|> sp

{-
instructionArg1 :: String -> Parsec String st Argument
instructionArg1 inst = do
  _ <- string inst >> spaces1
  argument
-}

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
    encode (RegArg t) (RegArg s) = return $ LD_R_R t s
    encode (RegArg t) (AddrArg AtHL) = return $ LD_R_ATHL t
    encode (RegArg t) (I8Arg n) = return $ LD_R_N t n
    encode (AddrArg AtHL) (RegArg s) = return $ LD_ATHL_R s
    encode (AddrArg AtHL) (I8Arg n) = return $ LD_ATHL_N n
    encode (RegArg ARegister) (AddrArg AtC) = return LD_A_ATC
    encode (RegArg ARegister) (AddrArg AtBC) = return LD_A_ATBC
    encode (RegArg ARegister) (AddrArg AtDE) = return LD_A_ATDE
    encode (RegArg ARegister) (AddrArg (AtNN nn)) = return $ LD_A_ATNN nn
    encode (AddrArg AtC) (RegArg ARegister) = return LD_ATC_A
    encode (AddrArg AtBC) (RegArg ARegister) = return LD_ATBC_A
    encode (AddrArg AtDE) (RegArg ARegister) = return LD_ATDE_A
    encode (AddrArg (AtNN nn)) (RegArg ARegister) = return $ LD_ATNN_A nn
    encode _ _ = fail "Invalid LD instruction"

load8dec :: Parsec String st Instruction
load8dec = do
    (t, s) <- instructionArg2 "LDD"
    encode t s
  where
    encode (RegArg ARegister) (AddrArg AtHL) = return LDD_A_ATHL
    encode (AddrArg AtHL) (RegArg ARegister) = return LDD_ATHL_A
    encode _ _ = fail "Invalid LDD instruction"

load8inc :: Parsec String st Instruction
load8inc = do
    (t, s) <- instructionArg2 "LDI"
    encode t s
  where
    encode (RegArg ARegister) (AddrArg AtHL) = return LDI_A_ATHL
    encode (AddrArg AtHL) (RegArg ARegister) = return LDI_ATHL_A
    encode _ _ = fail "Invalid LDI instruction"

loadh :: Parsec String st Instruction
loadh = do
    (t, s) <- instructionArg2 "LDH"
    encode t s
  where
    encode (RegArg ARegister) (AddrArg (AtN n)) = return $ LDH_A_ATN n
    encode (AddrArg (AtN n)) (RegArg ARegister) = return $ LDH_ATN_A n
    encode _ _ = fail "Invalid LDH instruction"

load16 :: Parsec String st Instruction
load16 = do
    (t, s) <- instructionArg2 "LD"
    encode t s
  where
    encode (RegPairArg BCRegPair) (I16Arg nn) = return $ LD_BC_NN nn
    encode (RegPairArg DERegPair) (I16Arg nn) = return $ LD_DE_NN nn
    encode (RegPairArg HLRegPair) (I16Arg nn) = return $ LD_HL_NN nn
    encode SPArg (I16Arg nn) = return $ LD_SP_NN nn
    encode _ _ = fail "Invalid LD instruction"

nop :: Parsec String st Instruction
nop = string "NOP" >> return NOP

stop :: Parsec String st Instruction
stop = string "STOP" >> return STOP

instruction :: Parsec String st Instruction
instruction
  = try load8
  <|> try load8dec
  <|> try load8inc
  <|> try loadh
  <|> try load16
  <|> try nop
  <|> stop

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
