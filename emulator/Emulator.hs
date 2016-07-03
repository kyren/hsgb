{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import qualified Data.Text as Text
import Linear
import qualified SDL

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]
  window <- SDL.createWindow "SDL Tutorial" SDL.defaultWindow
  SDL.showWindow window
  renderer <- SDL.createRenderer window (-1) (SDL.RendererConfig SDL.AcceleratedVSyncRenderer False)
  targetTexture <- SDL.createTexture renderer SDL.RGBA8888 SDL.TextureAccessStreaming (V2 160 144)

  mainLoop targetTexture renderer

  SDL.destroyTexture targetTexture
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit

mainLoop :: SDL.Texture -> SDL.Renderer -> IO ()
mainLoop targetTexture renderer = do
  SDL.copy renderer targetTexture Nothing Nothing

  SDL.present renderer

  events <- SDL.pollEvents
  let quit = elem SDL.QuitEvent $ map SDL.eventPayload events
  unless quit (mainLoop targetTexture renderer)
