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

load8Part :: Parsec String st Load8Part
load8Part = aRegister <|> bRegister <|> cRegister <|> dRegister <|> eRegister <|> hRegister <|> lRegister <|> atHL <|> immediate
  where
    aRegister = char 'A' >> return Load8A
    bRegister = char 'B' >> return Load8B
    cRegister = char 'C' >> return Load8C
    dRegister = char 'D' >> return Load8D
    eRegister = char 'E' >> return Load8E
    hRegister = char 'H' >> return Load8H
    lRegister = char 'L' >> return Load8L
    atHL = string "(HL)" >> return Load8AtHL
    immediate = liftM Load8I word8

load8 :: Parsec String st Instruction
load8 = do
  _ <- string "LD" >> spaces1
  t <- load8Part
  _ <- spaces >> char ',' >> spaces
  s <- load8Part
  return $ Load8 t s

nop :: Parsec String st Instruction
nop = string "NOP" >> return NoOp

stop :: Parsec String st Instruction
stop = string "STOP" >> return Stop

instruction :: Parsec String st Instruction
instruction = try nop <|> try stop <|> load8

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
  case mi of
    Just i ->
      if validInstruction i
         then return $ Just i
         else fail "Invalid Instruction"
    Nothing -> return Nothing

instructions :: Parsec String st [Instruction]
instructions = do
  res <- liftM catMaybes $ instructionLine `sepBy1` endOfLine
  optional endOfLine
  return res

parseInstructions :: String -> Either String [Instruction]
parseInstructions input = case parse instructions "" input of
  Left err -> Left (show err)
  Right result -> Right result

encodeInstructions :: [Instruction] -> Maybe (VU.Vector Word8)
encodeInstructions insts = VU.fromList <$> ops insts
  where
    ops (i:is) =
      case encodeInstruction i of
        Just os -> (os ++) <$> ops is
        Nothing -> Nothing
    ops [] = Just []
      

assemble :: String -> Either String (VU.Vector Word8)
assemble text = do
  inst <- parseInstructions text
  case encodeInstructions inst of
    Just ops -> Right ops
    Nothing -> Left "Invalid Instruction"
