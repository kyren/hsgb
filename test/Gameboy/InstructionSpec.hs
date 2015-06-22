module Gameboy.InstructionSpec (spec) where

import Data.Word
import Control.Monad.Trans.State
import Test.Hspec
import Test.QuickCheck
import Gameboy.Instructions

encodeInstructions :: [Instruction] -> [Word8]
encodeInstructions = concatMap encodeInstruction

decodeInstructions :: [Word8] -> Maybe [Instruction]
decodeInstructions ws = evalState go (ws, [])
  where
    go = do
      mi <- decodeInstruction getWord8
      (restBytes, insts) <- get
      case mi of
        Nothing -> return Nothing
        Just i -> return $ (\x -> insts ++ [i] ++ x) <$> decodeInstructions restBytes
    getWord8 = do
      (bytes, insts) <- get
      case bytes of 
        [] -> return Nothing
        (b:bs) -> do
          put (bs, insts)
          return $ Just b

checkInverses :: [Word8] -> Bool
checkInverses bs =
  case decodeInstructions bs of
    Nothing -> True
    Just is -> encodeInstructions is == bs

spec :: Spec
spec = describe "encoding and decoding" $
  it "are inverses of each other" $ property checkInverses
