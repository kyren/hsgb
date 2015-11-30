import qualified Data.ByteString as BS
import System.Environment
import Gameboy.Emulation
import Gameboy.Screen

main :: IO ()
main = do
  [romFile, stepCount] <- getArgs
  rom <- BS.readFile romFile
  let (Right state) = loadRom rom
  let state' = stepEmulator state (read stepCount)
  saveScreenShot "screen.png" (getScreen state')
  print state'
