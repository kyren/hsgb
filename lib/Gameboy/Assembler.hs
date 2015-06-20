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

data Addr
  = AtHL
  | AtC
  | AtBC
  | AtDE
  | AtNN Word16
  | AtN Word8

data Argument 
  = RegArg Register
  | RegPairArg RegisterPair
  | AddrArg Addr
  | I8Arg Word8
  | I16Arg Word16

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
    res <- hl <|> c <|> bc <|> de <|> try n <|> nn
    _ <- char ')'
    return res
  where
    hl = string "HL" >> return AtHL
    c = string "C" >> return AtC
    bc = string "BC" >> return AtBC
    de = string "DE" >> return AtDE
    n = AtN <$> immediate8
    nn = AtNN <$> immediate16

argument :: Parsec String st Argument
argument = (RegArg <$> register) <|> (RegPairArg <$> registerPair) <|> (AddrArg <$> addr) <|> (I8Arg <$> immediate8) <|> (I16Arg <$> immediate16)

load :: Parsec String st Instruction
load = do
    _ <- string "LD" >> spaces1
    t <- argument
    _ <- spaces >> char ',' >> spaces
    s <- argument
    encode t s
  where
    encode (RegArg t) (RegArg s) = return $ LD_R_R t s
    encode (RegArg t) (AddrArg AtHL) = return $ LD_R_ATHL t
    encode (RegArg t) (I8Arg n) = return $ LD_R_N t n
    encode (AddrArg AtHL) (RegArg s) = return $ LD_ATHL_R s
    encode (AddrArg AtHL) (I8Arg n) = return $ LD_ATHL_N n
    encode _ _ = fail "Invalid LD instruction"

nop :: Parsec String st Instruction
nop = string "NOP" >> return NOP

stop :: Parsec String st Instruction
stop = string "STOP" >> return STOP

instruction :: Parsec String st Instruction
instruction = try nop <|> try stop <|> load

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
