import Control.Monad
import Foreign
import Foreign.C
import qualified Graphics.UI.SDL as SDL
import Gameboy.Screen

screenWidth :: Int
screenWidth = 800

screenHeight :: Int
screenHeight = 720

checkSDL :: IO CInt -> IO ()
checkSDL op = do
  res <- op
  when (res /= 0) $ do
    errString <- SDL.getError >>= peekCString
    error errString

checkSDLPtr :: IO (Ptr a) -> IO (Ptr a)
checkSDLPtr op = do
  res <- op
  when (res == nullPtr) $ do
    errString <- SDL.getError >>= peekCString
    error errString
  return res

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

pixelGray :: Pixel -> CInt
pixelGray Black = 0
pixelGray DarkGray = 96
pixelGray LightGray = 192
pixelGray White = 255

drawScreen :: SDL.Renderer -> Screen -> IO ()
drawScreen renderer screen = alloca $ \rect -> sequence_ [drawPixel rect x y (pixelAt screen x y) | x <- [0 .. horizontalPixels - 1], y <- [0 .. verticalPixels - 1]]
  where
    drawPixel rect x y p = do
      let width = screenWidth `div` horizontalPixels
      let height = screenWidth `div` verticalPixels
      let xmin = x * width
      let ymin = y * height
      let gray = pixelGray p
      checkSDL $ SDL.setRenderDrawColor renderer (fromIntegral gray) (fromIntegral gray) (fromIntegral gray) 255
      poke rect (SDL.Rect (fromIntegral xmin) (fromIntegral ymin) (fromIntegral width) (fromIntegral height))
      checkSDL $ SDL.renderFillRect renderer rect
      return ()

mainLoop :: SDL.Renderer -> IO ()
mainLoop renderer = do
  checkSDL $ SDL.setRenderDrawColor renderer 0 0 0 255
  checkSDL $ SDL.renderClear renderer
  checkSDL $ SDL.setRenderDrawColor renderer 255 255 255 255
  drawScreen renderer testPattern
  SDL.renderPresent renderer
  quit <- processEvents
  unless quit (mainLoop renderer)

main :: IO ()
main = do
  checkSDL $ SDL.init SDL.SDL_INIT_VIDEO
  window <- checkSDLPtr $ withCString "SDL Tutorial" $ \title -> SDL.createWindow title SDL.SDL_WINDOWPOS_UNDEFINED SDL.SDL_WINDOWPOS_UNDEFINED (fromIntegral screenWidth) (fromIntegral screenHeight) SDL.SDL_WINDOW_OPENGL
  renderer <- checkSDLPtr $ SDL.createRenderer window (-1) (SDL.SDL_RENDERER_ACCELERATED .|. SDL.SDL_RENDERER_PRESENTVSYNC)

  SDL.showWindow window

  mainLoop renderer

  SDL.destroyWindow window
  SDL.quit
