{-# LANGUAGE ScopedTypeVariables #-}

module Gameboy.Assembler where

import Data.Maybe
import Data.Char
import Data.Word
import Control.Monad
import Text.Parsec
import Gameboy.CPU
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

register :: Parsec String st Register
register = aRegister <|> bRegister <|> cRegister <|> dRegister <|> eRegister <|> hRegister <|> lRegister <|> fRegister
  where
    aRegister = char 'A' >> return ARegister
    bRegister = char 'B' >> return BRegister
    cRegister = char 'C' >> return CRegister
    dRegister = char 'D' >> return DRegister
    eRegister = char 'E' >> return ERegister
    hRegister = char 'H' >> return HRegister
    lRegister = char 'L' >> return LRegister
    fRegister = char 'F' >> return FRegister

load8I :: Parsec String st Instruction
load8I = do
  _ <- string "LD"
  _ <- spaces1
  r <- register
  _ <- char ','
  _ <- spaces
  v <- word8
  return $ Load8I r v

instructionPart :: Parsec String st Instruction
instructionPart = load8I

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
