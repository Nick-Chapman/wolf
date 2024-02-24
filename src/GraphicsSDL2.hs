
module GraphicsSDL2
  ( Conf(..)
  , main
  ) where

import Control.Concurrent (threadDelay)
import Data.Map (Map)
import Prelude hiding (Int)
import Render (Colour(..),State,state0,render,dump)
import System.IO (hFlush,stdout)
import qualified Data.Map.Strict as Map (empty,insert,findWithDefault)
import qualified Data.Text as Text (pack)
import qualified Foreign.C.Types (CInt)
import qualified Prelude
import qualified Render (canvasSize)
import qualified Render as State (forwards,backwards,turnLeft,turnRight,strafeLeft,strafeRight)

import SDL (V2(..),Renderer,Rectangle(..),V2(..),V4(..),Point(P),($=),InputMotion(Pressed,Released))
import SDL.Font (Font,Color)
import SDL.Input.Keyboard.Codes
import qualified SDL
import qualified SDL.Font as Font (initialize,load,solid,size)

data Conf = Conf
  { scaleFactor :: Prelude.Int -- TODO: use CInt
  , fpsLimit :: Maybe Prelude.Int
  , showControls :: Bool
  } deriving Show

type Int = Foreign.C.Types.CInt

data World = World
  { paused :: Bool
  , w_showControls :: Bool
  , frameCount :: Int
  , buttons :: Buttons
  , state :: State
  , sf :: Int
  }

initWorld :: Int -> IO World
initWorld sf = do
  return $ World
    { paused = False
    , w_showControls = False -- TODO: from Config
    , frameCount = 0
    , buttons = buttons0
    , state = state0
    , sf -- TODO: chosse default here
    }

main :: Conf -> IO ()
main Conf{scaleFactor,fpsLimit} = do
  putStrLn "*GraphicsSDL2*"
  let sf = fromIntegral scaleFactor
  world <- initWorld sf
  SDL.initializeAll
  Font.initialize
  let winConfig = SDL.defaultWindow
  win <- SDL.createWindow (Text.pack "Wolf") $ winConfig
  renderer <- SDL.createRenderer win (-1) SDL.defaultRenderer
  font <- Font.load "assets/Acorn Full Nostalgia.ttf" (5 * scaleFactor) -- TODO: dynamic sf
  let _flush = hFlush stdout
  let assets = DrawAssets { win, renderer, font, canvasSize = Render.canvasSize }
  let
    loop :: World -> IO () -- TODO: extract loop
    loop world = do
      before <- SDL.ticks
      --putStr "."; _flush
      events <- SDL.pollEvents
      processEvents world events >>= \case
        Nothing -> return () -- quit
        Just world -> do
          resize world assets
          drawEverything assets world
          world <- stepFrame world
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
  loop world
  SDL.destroyRenderer renderer
  SDL.destroyWindow win
  SDL.quit

stepFrame :: World -> IO World
stepFrame world@World{paused,frameCount,buttons,state} = do -- TODO: inline
  --putStrLn $ "FRAME: " ++ show frameCount
  pure $ if paused then world else do
    world
      { frameCount = 1 + frameCount
      , state = updateState buttons state
      }

updateState :: Buttons -> State -> State
updateState b s =
  ( (if getB Forwards b then State.forwards else id)
  . (if getB Backwards b then State.backwards else id)
  . (if getB TurnLeft b then State.turnLeft else id)
  . (if getB TurnRight b then State.turnRight else id)
  . (if getB StrafeLeft b then State.strafeLeft else id)
  . (if getB StrafeRight b then State.strafeRight else id)
  ) s

processEvents :: World -> [SDL.Event] -> IO (Maybe World)
processEvents world = \case
  [] -> pure (Just world)
  e1:es -> do
    case xEvent e1 of
      Nothing -> processEvents world es
      Just (key,motion) -> do
        updateKey key motion world >>= \case
          Just world -> processEvents world es
          Nothing -> pure Nothing -- quit
  where
    xEvent :: SDL.Event -> Maybe (Keycode, InputMotion) -- TODO: inline
    xEvent = \case
      SDL.Event _t SDL.QuitEvent -> Nothing -- TODO: respond to window quit
      SDL.Event _ (SDL.KeyboardEvent ke) -> xKeyboundEvent ke
      SDL.Event _ _ -> Nothing

    xKeyboundEvent :: SDL.KeyboardEventData -> Maybe (Keycode, InputMotion) -- TODO: inline
    xKeyboundEvent ke = do
      let key = SDL.keysymKeycode (SDL.keyboardEventKeysym ke)
      let motion = SDL.keyboardEventKeyMotion ke
      Just (key, motion)

data KeyAction -- TODO: does this have much value?
  = NoAction
  | Quit
  | Drive But InputMotion
  | TogglePause
  | ToggleControlDisplay
  | IncreaseSF
  | DecreaseSF
  | Dump
  deriving (Show)

keyMapping :: (Keycode,InputMotion) -> KeyAction
keyMapping = \case
  (KeycodeReturn,Pressed) -> Dump
  (KeycodeEscape,Pressed) -> Quit
  (KeycodeDelete,Pressed) -> TogglePause
  (KeycodeSpace,Pressed) -> ToggleControlDisplay
  (KeycodeEquals,Pressed) -> IncreaseSF
  (KeycodeMinus,Pressed) -> DecreaseSF
  (KeycodeLeft,m) -> Drive StrafeLeft m
  (KeycodeRight,m) -> Drive StrafeRight m
  (KeycodeA,m) -> Drive TurnLeft m
  (KeycodeS,m) -> Drive Backwards m
  (KeycodeD,m) -> Drive TurnRight m
  (KeycodeW,m) -> Drive Forwards m
  _ -> NoAction

updateKey :: Keycode -> InputMotion -> World -> IO (Maybe World)
updateKey key motion w@World{w_showControls,buttons,paused,sf,state} =
  case keyMapping (key,motion) of
    NoAction -> pure $ Just w
    Quit -> pure $ Nothing
    TogglePause -> pure $ Just w { paused = not (paused) }
    ToggleControlDisplay -> pure $ Just w { w_showControls = not w_showControls }
    Drive but motion -> pure $ Just w { buttons = drive motion but buttons }
    IncreaseSF -> pure $ Just w { sf = sf+1 }
    DecreaseSF -> pure $ Just w { sf = max (sf-1) 1 }
    Dump -> do Render.dump state; pure (Just w)
  where
    drive :: InputMotion -> But -> Buttons -> Buttons
    drive = \case Pressed -> pressB; Released -> releaseB

----------------------------------------------------------------------
-- DrawAssets

data DrawAssets = DrawAssets
  { renderer :: Renderer
  , win :: SDL.Window
  , font :: Font
  , canvasSize :: (Int,Int)
  }

resize :: World -> DrawAssets -> IO ()
resize World{sf,w_showControls} DrawAssets{canvasSize,win} = do
  let (screenW,screenH) = canvasSize
  let windowSize = V2 w h where
        w = sf * (screenW + if w_showControls then 200 else 0)
        h = sf * screenH
  SDL.windowSize win $= windowSize

drawEverything :: DrawAssets -> World -> IO ()
drawEverything assets@DrawAssets{renderer=r} world = do
  setColor r DarkGrey
  SDL.clear r
  renderPicture assets world (pictureWorld world)
  SDL.present r

renderPicture :: DrawAssets -> World -> Picture  -> IO ()
renderPicture a@DrawAssets{renderer=r} World{sf} picture = do
  traverse picture
  where
    scale :: Int -> Int
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
            (w,_) = Render.canvasSize

renderText :: DrawAssets -> Colour -> String -> Point V2 Int -> IO ()
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
  Magenta -> V4 m 0 m m
  where
    m = 255

----------------------------------------------------------------------
-- Picture

data Picture
  = Pictures [Picture]
  | Text { string :: String, lineNo :: Int, emphasized :: Bool }
  | Pixel { x :: Int, y :: Int, col :: Colour }

pictureWorld :: World -> Picture
pictureWorld w@World{w_showControls,frameCount,state} =
  Pictures
  [ pictureState state
  , if w_showControls then controls else Pictures []
  ]
  where
    controls = Pictures
      [ pictureButtons w
      , Text { lineNo = 1, string = "frame : " <> show frameCount, emphasized = False }
      ]

pictureState :: State -> Picture
pictureState s= Pictures
  [ Text { lineNo = 2, string = show s, emphasized = False }
  , pictureCanvas s
  ]

pictureCanvas :: State -> Picture
pictureCanvas s = Pictures [ Pixel x y col | ((x,y),col) <- render s]

pictureButtons :: World -> Picture -- TOOD: only show the buttons
pictureButtons World{buttons,paused} =
  Pictures [ Text { lineNo, string = describeKeyAndMapping key, emphasized }
           | (lineNo,key) <- zip [3..] keys
           , let emphasized = do
                   let action = keyMapping (key,Pressed)
                   case action of
                     TogglePause -> paused
                     Drive but _ -> getB but buttons
                     _ -> False
           ]
  where
    keys =
      [ KeycodeEscape,
        KeycodeDelete,
        KeycodeSpace,
        KeycodeEquals,
        KeycodeMinus,
        KeycodeLeft,
        KeycodeRight,
        KeycodeA,
        KeycodeS,
        KeycodeD,
        KeycodeW ]

describeKeyAndMapping :: Keycode -> String
describeKeyAndMapping key = show key <> " : " <> show (keyMapping (key,Pressed))

----------------------------------------------------------------------
-- Buttons

data But
  = Forwards
  | Backwards
  | TurnLeft
  | TurnRight
  | StrafeLeft
  | StrafeRight
  deriving (Eq,Ord,Show)

newtype Buttons = Buttons { map :: Map But Bool } deriving Show

buttons0 :: Buttons
buttons0 = Buttons { map = Map.empty }

getB :: But -> Buttons -> Bool
getB but Buttons{map} = Map.findWithDefault False but map

setB :: Bool -> But -> Buttons -> Buttons
setB v but Buttons{map} = Buttons { map = Map.insert but v map }

pressB :: But -> Buttons -> Buttons
pressB = setB True

releaseB :: But -> Buttons -> Buttons
releaseB = setB False
