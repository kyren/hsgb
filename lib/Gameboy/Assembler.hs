{-# LANGUAGE ScopedTypeVariables #-}

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

spaces1 :: Parsec String st String
spaces1 = many1 space

positiveNatural :: Parsec String st Integer
positiveNatural = do
  digits <- many1 digit
  return $ foldl (\a i -> a * 10 + fromIntegral (digitToInt i)) 0 digits

positiveInteger :: forall st a . (Integral a, Bounded a) => Parsec String st a
positiveInteger = do
  value <- positiveNatural
  when (value > fromIntegral (maxBound :: a)) $ fail "Out of bounds integer"
  return $ fromIntegral value

word8 :: Parsec String st Word8
word8 = positiveInteger

load8Target :: Parsec String st Load8Target
load8Target = aRegister <|> bRegister <|> cRegister <|> dRegister <|> eRegister <|> hRegister <|> lRegister <|> atHL
  where
    aRegister = char 'A' >> return Load8A
    bRegister = char 'B' >> return Load8B
    cRegister = char 'C' >> return Load8C
    dRegister = char 'D' >> return Load8D
    eRegister = char 'E' >> return Load8E
    hRegister = char 'H' >> return Load8H
    lRegister = char 'L' >> return Load8L
    atHL = string "(HL)" >> return Load8AtHL

load8I :: Parsec String st Instruction
load8I = do
  _ <- string "LD" >> spaces1
  r <- load8Target
  _ <- spaces >> char ',' >> spaces
  v <- word8
  return $ Load8I r v

load8 :: Parsec String st Instruction
load8 = do
  _ <- string "LD" >> spaces1
  t <- load8Target
  _ <- spaces >> char ',' >> spaces
  s <- load8Target
  return $ Load8 t s

nop :: Parsec String st Instruction
nop = string "NOP" >> return NoOp

stop :: Parsec String st Instruction
stop = string "STOP" >> return Stop

instructionPart :: Parsec String st Instruction
instructionPart = try load8I <|> try load8 <|> try nop <|> stop

comment :: Parsec String st ()
comment = do
  _ <- char ';'
  _ <- many (noneOf "\r\n")
  return ()

endLinePart :: Parsec String st ()
endLinePart = do
  skipMany (oneOf " \t")
  optional comment
  _ <- string "\r\n" <|> string "\n"
  return ()

instructionLine :: Parsec String st (Maybe Instruction)
instructionLine = do
  skipMany (oneOf " \t")
  instruction <- instructionPart
  endLinePart
  return $ Just instruction

blankLine :: Parsec String st (Maybe Instruction)
blankLine = do
  endLinePart
  return Nothing

instructionOrBlankLine :: Parsec String st (Maybe Instruction)
instructionOrBlankLine = try blankLine <|> instructionLine

instructions :: Parsec String st [Instruction]
instructions = do
  res <- liftM catMaybes $ many instructionOrBlankLine
  eof
  return res

parseInstructions :: String -> Either String [Instruction]
parseInstructions input = case parse instructions "" input of
  Left err -> Left (show err)
  Right result -> Right result

encodeInstructions :: [Instruction] -> VU.Vector Word8
encodeInstructions is = VU.fromList $ concatMap encodeInstruction is

assemble :: String -> Either String (VU.Vector Word8)
assemble text = do
  inst <- parseInstructions text
  return $ encodeInstructions inst
