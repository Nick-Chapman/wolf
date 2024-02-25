
module GameLoop
  ( Conf(..)
  , main
  ) where

import Control.Concurrent (threadDelay)
import Data.Map (Map)
import GHC.Word (Word8)
import Prelude hiding (Int)
import Render (Pix,Colour(..),State,state0,render,dump)
import System.IO (hFlush,stdout)
import qualified Data.Map.Strict as Map (empty,insert,findWithDefault)
import qualified Data.Text as Text (pack)
import qualified Foreign.C.Types (CInt)
import qualified Prelude
import qualified Render (canvasSize)
import qualified Render as State (forwards,backwards,turnLeft,turnRight,strafeLeft,strafeRight)

import SDL (V2(..),Renderer,Rectangle(..),V2(..),V4(..),Point(P),($=),InputMotion(Pressed,Released))
import SDL.Input.Keyboard.Codes
import qualified SDL

data Conf = Conf
  { sf0 :: Prelude.Int -- TODO: use CInt
  , fps0 :: Prelude.Int
  } deriving Show

type Int = Foreign.C.Types.CInt

data World = World
  { paused :: Bool
  , frameCount :: Int
  , buttons :: Buttons
  , state :: State
  , sf :: Int
  , fps :: Int
  }

initWorld :: Conf -> IO World
initWorld Conf{sf0,fps0} = do
  return $ World
    { paused = False
    , frameCount = 0
    , buttons = buttons0
    , state = state0
    , sf = fromIntegral sf0
    , fps = fromIntegral fps0
    }

main :: Conf -> IO ()
main conf = do
  world <- initWorld conf
  SDL.initializeAll
  let winConfig = SDL.defaultWindow
  win <- SDL.createWindow (Text.pack "Wolf") $ winConfig
  renderer <- SDL.createRenderer win (-1) SDL.defaultRenderer
  let _flush = hFlush stdout
  let assets = DrawAssets { win, renderer, canvasSize = Render.canvasSize }
  let
    loop :: World -> IO () -- TODO: extract loop
    loop world@World{fps} = do
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
                let durationMs = fromIntegral (1000*(after-before))
                let goalMs = 1000000 `div` fromIntegral fps
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
  | IncreaseSF
  | DecreaseSF
  | Dump
  deriving (Show)

keyMapping :: (Keycode,InputMotion) -> KeyAction
keyMapping = \case
  (KeycodeReturn,Pressed) -> Dump
  (KeycodeEscape,Pressed) -> Quit
  (KeycodeDelete,Pressed) -> TogglePause
  -- (KeycodeSpace,Pressed) -> undefined
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
updateKey key motion w@World{buttons,paused,sf,state} =
  case keyMapping (key,motion) of
    NoAction -> pure $ Just w
    Quit -> pure $ Nothing
    TogglePause -> pure $ Just w { paused = not (paused) }
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
  , canvasSize :: (Int,Int)
  }

resize :: World -> DrawAssets -> IO ()
resize World{sf} DrawAssets{canvasSize,win} = do
  let (screenW,screenH) = canvasSize
  let windowSize = V2 w h where
        w = sf * screenW
        h = sf * screenH
  SDL.windowSize win $= windowSize

drawEverything :: DrawAssets -> World -> IO ()
drawEverything assets@DrawAssets{renderer=r} world@World{state} = do
  setColor r DarkGrey
  SDL.clear r
  renderPicture assets world (render state)
  SDL.present r

renderPicture :: DrawAssets -> World -> [Pix]  -> IO ()
renderPicture DrawAssets{renderer=r} World{sf} pictures = do
  mapM_ traverse pictures
  where
    scale :: Int -> Int
    scale x = sf * x

    traverse :: Pix -> IO ()
    traverse = \case

      ((x0,y0),col) -> do
        setColor r col
        let x = scale (fromIntegral x0)
        let y = scale (fromIntegral y0)
        let rect = SDL.Rectangle (SDL.P (V2 x y)) (V2 sf sf)
        SDL.fillRect r (Just rect)

setColor :: SDL.Renderer -> Colour -> IO ()
setColor r c = SDL.rendererDrawColor r $= color c

color :: Colour -> V4 Word8
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
