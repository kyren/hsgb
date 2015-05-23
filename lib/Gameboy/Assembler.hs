{-# LANGUAGE ScopedTypeVariables #-}

module Gameboy.Assembler where

import Data.Maybe
import Data.Char
import Data.Word
import Control.Monad
import Text.Parsec
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

load8Register :: Parsec String st LoadRegister
load8Register = bRegister <|> cRegister <|> dRegister <|> eRegister <|> hRegister <|> lRegister
  where
    bRegister = char 'B' >> return LoadB
    cRegister = char 'C' >> return LoadC
    dRegister = char 'D' >> return LoadD
    eRegister = char 'E' >> return LoadE
    hRegister = char 'H' >> return LoadH
    lRegister = char 'L' >> return LoadL

load8I :: Parsec String st Instruction
load8I = do
  _ <- string "LD" >> spaces1
  r <- load8Register
  _ <- spaces >> char ',' >> spaces
  v <- word8
  return $ Load8I r v

load8Target :: Parsec String st Load8Target
load8Target = regTarget <|> athlTarget
  where
    regTarget = do
      r <- load8Register
      return $ Load8TargetRegister r
    athlTarget = do
      _ <- string "(HL)"
      return Load8TargetAtHL

load8 :: Parsec String st Instruction
load8 = do
  _ <- string "LD" >> spaces1
  t <- load8Target
  _ <- spaces >> char ',' >> spaces
  s <- load8Target
  return $ Load8 t s

instructionPart :: Parsec String st Instruction
instructionPart = try load8I <|> try load8

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
