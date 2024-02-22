
module GraphicsSDL
  ( Conf(..)
  , main
  ) where

import Control.Concurrent (threadDelay)
import Data.List.Extra (groupSort)
import Data.Map (Map)
import Foreign.C.Types (CInt)
import Render (Colour(..),canvasSize)
import SDL (V2(..),Renderer,Rectangle(..),V2(..),V4(..),Point(P),($=))
import SDL.Font (Font,Color)
import System.IO (hFlush,stdout)
import World (World,Key(..),KeyMotion(Down,Up),Picture(..))
import qualified Data.Map.Strict as Map (fromList,lookup)
import qualified Data.Text as Text (pack)
import qualified SDL
import qualified SDL.Font as Font (initialize,load,solid,size)
import qualified World (initWorld,updateKey,stepFrame,pictureWorld)

data Conf = Conf
  { scaleFactor :: Int
  , fpsLimit :: Maybe Int
  , showControls :: Bool
  }

main :: Conf -> IO ()
main Conf{scaleFactor,fpsLimit,showControls} = do

  let sf = fromIntegral scaleFactor

  SDL.initializeAll
  Font.initialize

  let (screenW,screenH) = canvasSize

  let windowSize = V2 w h where
        w = sf * (screenW + if showControls then 200 else 0)
        h = sf * screenH

  let winConfig = SDL.defaultWindow { SDL.windowInitialSize = windowSize }
  win <- SDL.createWindow (Text.pack "Wolf") $ winConfig
  renderer <- SDL.createRenderer win (-1) SDL.defaultRenderer
  font <- Font.load "assets/Acorn Full Nostalgia.ttf" (5 * scaleFactor)

  let _flush = hFlush stdout
  let assets = DrawAssets { renderer, font, sf }

  let
    loop :: World -> IO ()
    loop world = do
      before <- SDL.ticks
      --putStr "."; _flush
      events <- SDL.pollEvents
      case processEvents world events of
        Nothing -> return () -- quit
        Just world -> do

          drawEverything assets world
          world <- World.stepFrame world
          maybeDelay
          loop world

            where
              maybeDelay = do
                after <- SDL.ticks
                case fpsLimit of
                  Nothing -> return ()
                  Just fpsLimit -> do
                    let durationMs = fromIntegral (1000*(after-before))
                    let goalMs = 1000000 `div` fpsLimit

                    if (goalMs > durationMs)
                      then threadDelay (goalMs - durationMs)
                      else return ()

  world <- World.initWorld -- rom
  loop world

  SDL.destroyRenderer renderer
  SDL.destroyWindow win
  SDL.quit

processEvents :: World -> [SDL.Event] -> Maybe World
processEvents world = \case
  [] -> Just world
  e1:es -> do
    case xEvent e1 of
      Nothing -> processEvents world es
      Just (key,motion) ->
        case World.updateKey key motion world of
          Just world -> processEvents world es
          Nothing -> Nothing -- quit
  where
    xEvent :: SDL.Event -> Maybe (Key, KeyMotion)
    xEvent = \case
      SDL.Event _t SDL.QuitEvent -> Nothing
      SDL.Event _ (SDL.KeyboardEvent ke) -> xKeyboundEvent ke
      SDL.Event _ _ -> Nothing

    xKeyboundEvent :: SDL.KeyboardEventData -> Maybe (Key, KeyMotion)
    xKeyboundEvent ke = do
      let code = SDL.keysymKeycode (SDL.keyboardEventKeysym ke)
      case Map.lookup code keyMapTable of
        Nothing -> Nothing
        Just key -> do
          let motion = SDL.keyboardEventKeyMotion ke
          Just (key, xMotion motion)
      where
        xMotion = \case SDL.Pressed -> World.Down; SDL.Released -> World.Up


keyMapTable :: Map SDL.Keycode Key
keyMapTable = Map.fromList ys
  where
    xs = [ (keyedBy key, key) | key <- [minBound..maxBound] ]
    ys = [ (code, expectUnique code keys) | (code,keys) <- groupSort xs ]
    expectUnique code = \case
      [key] -> key
      keys -> error $
        unlines $
        ("bad keyMapTable: " <> show code) : [ "--> " <> show key | key <- keys ]

    -- | define the reverse mapping to be sure we are complete
    keyedBy :: Key -> SDL.Keycode
    keyedBy = \case
      KeyEscape -> SDL.KeycodeEscape
      KeyDelete -> SDL.KeycodeDelete
      KeySpace -> SDL.KeycodeSpace
      KeyLeft -> SDL.KeycodeLeft
      KeyRight -> SDL.KeycodeRight
      KeyA -> SDL.KeycodeA
      KeyS -> SDL.KeycodeS
      KeyD -> SDL.KeycodeD
      KeyW -> SDL.KeycodeW

data DrawAssets = DrawAssets
  { renderer :: Renderer
  , font :: Font
  , sf :: CInt -- scale factor
  }

drawEverything :: DrawAssets -> World -> IO ()
drawEverything assets@DrawAssets{renderer=r} world = do
  setColor r DarkGrey
  SDL.clear r
  --setColor r Yellow
  renderPicture assets (World.pictureWorld world)
  SDL.present r

renderPicture :: DrawAssets -> Picture  -> IO ()
renderPicture a@DrawAssets{renderer=r,sf} = traverse
  where
    scale :: CInt -> CInt
    scale x = sf * x

    traverse :: Picture -> IO ()
    traverse = \case
      Pictures pics -> mapM_ traverse pics

      Pixel{x=x0,y=y0,col} -> do
        setColor r col
        let x = scale (fromIntegral x0)
        let y = scale (fromIntegral y0)
        let rect = SDL.Rectangle (SDL.P (V2 x y)) (V2 sf sf)
        SDL.fillRect r (Just rect)

      Text{lineNo,string,emphasized} -> do
        renderText a col string (P (V2 (scale x) (scale y)))
          where
            col = if emphasized then Green else White
            x = w+10
            y = fromIntegral lineNo * 10
            (w,_) = canvasSize

renderText :: DrawAssets -> Colour -> String -> Point V2 CInt -> IO ()
renderText DrawAssets{renderer=r,font} col string pos = do
  let text = Text.pack string
  surface <- Font.solid font (color col) text
  texture <- SDL.createTextureFromSurface r surface
  SDL.freeSurface surface
  (fw,fh) <- Font.size font text
  let (w,h) = (fromIntegral fw, fromIntegral fh)
  SDL.copy r texture Nothing (Just (Rectangle pos (V2 w h)))
  SDL.destroyTexture texture

setColor :: SDL.Renderer -> Colour -> IO ()
setColor r c = SDL.rendererDrawColor r $= color c

color :: Colour -> Color
color = \case
  DarkGrey -> V4 20 20 20 m
  LightGrey -> V4 200 200 200 m
  Black -> V4 0 0 0 m
  White -> V4 m m m m
  Red -> V4 m 0 0 m
  Green -> V4 0 m 0 m
  Blue -> V4 0 0 m m
  Yellow -> V4 m m 0 m
  where
    m = 255
