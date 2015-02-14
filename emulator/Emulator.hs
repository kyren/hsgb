import Control.Monad
import Foreign
import Foreign.C
import qualified Graphics.UI.SDL as SDL

checkSDLError :: Bool -> IO ()
checkSDLError cond = when cond $ do
  errString <- SDL.getError >>= peekCString
  error errString

processEvents :: IO Bool
processEvents = alloca $ \eventPtr -> do
  res <- SDL.pollEvent eventPtr
  if res == 0
     then return False
     else do
       event <- peek eventPtr
       case event of
            (SDL.QuitEvent _ _) -> return True
            _ -> return False

mainLoop :: IO ()
mainLoop = do
  quit <- processEvents
  unless quit mainLoop

main :: IO ()
main = do
  res <- SDL.init SDL.SDL_INIT_VIDEO
  checkSDLError (res /= 0)

  window <- withCString "SDL Tutorial" $ \title -> SDL.createWindow title SDL.SDL_WINDOWPOS_UNDEFINED SDL.SDL_WINDOWPOS_UNDEFINED 800 600 SDL.SDL_WINDOW_OPENGL
  checkSDLError (window == nullPtr)

  renderer <- SDL.createRenderer window (-1) (SDL.SDL_RENDERER_ACCELERATED .|. SDL.SDL_RENDERER_PRESENTVSYNC)
  checkSDLError (renderer == nullPtr)

  SDL.showWindow window

  mainLoop

  SDL.destroyWindow window
  SDL.quit
