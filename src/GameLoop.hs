
module GameLoop
  ( initWorld, World
  , main
  ) where

import Control.Concurrent (threadDelay)
import Data.Map (Map)
import GHC.Word (Word8)
import Prelude hiding (Int)
import Render (Pix,Colour(..),State,state0,render)
import System.IO (hFlush,stdout)
import qualified Data.Map.Strict as Map (empty,insert,findWithDefault)
import qualified Data.Text as Text (pack)
import qualified Foreign.C.Types (CInt)
import qualified Render (canvasSize,fps,incAA,decAA,selectAA)
import qualified Render as State (forwards,backwards,turnLeft,turnRight,strafeLeft,strafeRight)

import SDL (Renderer,Rectangle(..),V2(..),V4(..),Point(P),($=),InputMotion(Pressed,Released))
import SDL.Input.Keyboard.Codes
import qualified SDL

type Int = Foreign.C.Types.CInt

data World = World
  { paused :: Bool
  , frameCount :: Int
  , buttons :: Buttons
  , state :: State
  , sf :: Int
  }

initWorld :: World
initWorld = World
  { paused = False
  , frameCount = 0
  , buttons = buttons0
  , state = state0
  , sf = 2
  }

main :: World -> IO ()
main world = do
  SDL.initializeAll
  let winConfig = SDL.defaultWindow
  win <- SDL.createWindow (Text.pack "Wolf") $ winConfig
  renderer <- SDL.createRenderer win (-1) SDL.defaultRenderer
  let _flush = hFlush stdout
  let assets = DrawAssets { win, renderer }
  let
    loop :: World -> IO () -- TODO: extract loop
    loop world@World{state} = do
      before <- SDL.ticks
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
                let durationMs = fromIntegral (1000*(after-before))
                let goalMs = 1000000 `div` fromIntegral (Render.fps state)
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
  ( (if isPressed Forwards b then State.forwards else id)
  . (if isPressed Backwards b then State.backwards else id)
  . (if isPressed TurnLeft b then State.turnLeft else id)
  . (if isPressed TurnRight b then State.turnRight else id)
  . (if isPressed StrafeLeft b then State.strafeLeft else id)
  . (if isPressed StrafeRight b then State.strafeRight else id)
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
  | IncreaseSF
  | DecreaseSF
  | IncAA
  | DecAA
  | SelectAA
  deriving (Show)

keyMapping :: (Keycode,InputMotion) -> KeyAction
keyMapping = \case
  (KeycodeEscape,Pressed) -> Quit
  (KeycodeDelete,Pressed) -> TogglePause

  (KeycodeReturn,Pressed) -> SelectAA
  (KeycodeUp,Pressed) -> IncAA
  (KeycodeDown,Pressed) -> DecAA

  (KeycodeEquals,Pressed) -> IncreaseSF
  (KeycodeMinus,Pressed) -> DecreaseSF

  (KeycodeA,m) -> Drive TurnLeft m
  (KeycodeS,m) -> Drive Backwards m
  (KeycodeD,m) -> Drive TurnRight m
  (KeycodeW,m) -> Drive Forwards m
  (KeycodeLeft,m) -> Drive StrafeLeft m
  (KeycodeRight,m) -> Drive StrafeRight m
  _ -> NoAction

updateKey :: Keycode -> InputMotion -> World -> IO (Maybe World)
updateKey key motion w@World{buttons,paused,sf,state} =
  case keyMapping (key,motion) of
    NoAction -> pure $ Just w
    Quit -> pure $ Nothing
    TogglePause -> pure $ Just w { paused = not (paused) }
    Drive but motion -> pure $ Just w { buttons = setButton motion but buttons }
    IncreaseSF -> pure $ Just w { sf = sf+1 }
    DecreaseSF -> pure $ Just w { sf = max (sf-1) 1 }
    IncAA -> dump w { state = Render.incAA state }
    DecAA -> dump w { state = Render.decAA state }
    SelectAA -> dump w { state = Render.selectAA state }
    where
      dump w@World{state} = do print state; pure (Just w)

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

newtype Buttons = Buttons { map :: Map But InputMotion } deriving Show

buttons0 :: Buttons
buttons0 = Buttons { map = Map.empty }

isPressed :: But -> Buttons -> Bool
isPressed but Buttons{map} = Map.findWithDefault Released but map == Pressed

setButton :: InputMotion -> But -> Buttons -> Buttons
setButton v but Buttons{map} = Buttons { map = Map.insert but v map }

----------------------------------------------------------------------
-- DrawAssets

data DrawAssets = DrawAssets
  { renderer :: Renderer
  , win :: SDL.Window
  }

resize :: World -> DrawAssets -> IO ()
resize World{sf,state} DrawAssets{win} = do
  let (w,h) = Render.canvasSize state
  let windowSize = V2 (sf * w) (sf * h)
  SDL.windowSize win $= windowSize

drawEverything :: DrawAssets -> World -> IO ()
drawEverything assets@DrawAssets{renderer=r} world@World{state} = do
  setColor r DarkGrey
  SDL.clear r
  renderPicture assets world (render state)
  SDL.present r

renderPicture :: DrawAssets -> World -> [Pix]  -> IO ()
renderPicture DrawAssets{renderer=r} World{sf} pixs = do
  mapM_ traverse pixs
  where
    traverse :: Pix -> IO ()
    traverse ((x0,y0),col) = do
      setColor r col
      let x = sf * fromIntegral x0
      let y = sf * fromIntegral y0
      let rect = SDL.Rectangle (SDL.P (V2 x y)) (V2 sf sf)
      SDL.fillRect r (Just rect)

setColor :: SDL.Renderer -> Colour -> IO ()
setColor r c = SDL.rendererDrawColor r $= color c

color :: Colour -> V4 Word8
color = \case
  DarkGrey -> V4 20 20 20 x
  LightGrey -> V4 200 200 200 x
  Black -> V4 0 0 0 x
  White -> V4 x x x x
  Red -> V4 x 0 0 x
  Green -> V4 0 x 0 x
  Blue -> V4 0 0 x x
  Yellow -> V4 x x 0 x
  Magenta -> V4 x 0 x x
  where x = 255
