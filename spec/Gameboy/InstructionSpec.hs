module Gameboy.InstructionSpec (spec) where

import Data.Word
import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Test.Hspec
import Test.QuickCheck
import Gameboy.Instructions

encodeInstructions :: [Instruction] -> [Word8]
encodeInstructions = concatMap encodeInstruction

decodeInstructions :: [Word8] -> Maybe [Instruction]
decodeInstructions [] = Just []
decodeInstructions ws = evalStateT go ws
  where
    go = do
      mi <- decodeInstruction getWord8
      restBytes <- get
      case mi of
        Nothing -> lift Nothing
        Just i -> lift $ (i:) <$> decodeInstructions restBytes
    getWord8 = do
      bytes <- get
      case bytes of 
        [] -> lift Nothing
        (b:bs) -> do
          put bs
          lift $ Just b

checkInverses :: [Word8] -> Bool
checkInverses bs =
  case decodeInstructions bs of
    Nothing -> True
    Just is -> encodeInstructions is == bs

spec :: Spec
spec = describe "encoding and decoding" $
  it "are inverses of each other" $ property checkInverses
