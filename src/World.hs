
module World
  ( World, initWorld, stepFrame
  , Key(..), KeyMotion(..), updateKey
  , Picture(..), pictureWorld
  ) where

import Buttons (Buttons,buttons0,But)
import qualified Buttons as But (get,press,release,toggle,But(..))
import Render (Colour(..),State,state0,render)
import qualified Render as State (forwards,backwards,turnLeft,turnRight,strafeLeft,strafeRight)

import Prelude hiding (Int)
import Foreign.C.Types (CInt)
type Int = CInt

data Key
  = KeyEscape
  | KeyDelete
  | KeySpace
  | KeyLeft
  | KeyRight
  | KeyW
  | KeyS
  | KeyA
  | KeyD
  deriving (Enum,Bounded)

data KeyMotion = Down | Up

data KeyAction
  = NoAction
  | Drive But KeyMotion
  | Toggle But
  | TogglePause
  | ToggleControlDisplay
  | Quit

data World = World
  { paused :: Bool
  , showControls :: Bool
  , frameCount :: Int
  , buttons :: Buttons
  , state :: State
  }

initWorld :: IO World
initWorld = do
  return $ World
    { paused = False
    , showControls = True
    , frameCount = 0
    , buttons = buttons0
    , state = state0
    }

stepFrame :: World -> IO World
stepFrame world@World{paused,frameCount,buttons,state} = do
  --putStrLn $ "FRAME: " ++ show frameCount
  pure $ if paused then world else do
    world
      { frameCount = 1 + frameCount
      , state = updateState buttons state
      }

updateKey :: Key -> KeyMotion -> World -> Maybe World
updateKey key motion w@World{showControls,buttons,paused} =
  case keyMapping (key,motion) of
    NoAction -> Just w
    Quit -> Nothing
    TogglePause -> Just $ w { paused = not (paused) }
    ToggleControlDisplay -> Just $ w { showControls = not showControls }
    Drive but motion -> Just $ w { buttons = drive motion but buttons }
    Toggle but -> Just $ w { buttons = But.toggle but buttons }
  where
    drive :: KeyMotion -> But -> Buttons -> Buttons
    drive = \case Down -> But.press; Up -> But.release

keyMapping :: (Key,KeyMotion) -> KeyAction
keyMapping = \case
  (KeyEscape,Down) -> Quit
  (KeyDelete,Down) -> TogglePause
  (KeySpace,Down) -> ToggleControlDisplay
  (KeyLeft,m) -> Drive But.StrafeLeft m
  (KeyRight,m) -> Drive But.StrafeRight m
  (KeyA,m) -> Drive But.TurnLeft m
  (KeyS,m) -> Drive But.Backwards m
  (KeyD,m) -> Drive But.TurnRight m
  (KeyW,m) -> Drive But.Forwards m
  _ -> NoAction

data Picture
  = Pictures [Picture]
  | Text { string :: String, lineNo :: Int, emphasized :: Bool }
  | Pixel { x :: Int, y :: Int, col :: Colour }

pictureWorld :: World -> Picture
pictureWorld w@World{showControls,frameCount,state} =
  Pictures
  [ pictureState state
  , if showControls then controls else Pictures []
  ]
  where
    controls = Pictures
      [ pictureButtons w
      , Text { lineNo = 1, string = "frame : " <> show frameCount, emphasized = False }
      ]

pictureButtons :: World -> Picture
pictureButtons World{buttons,paused} =
  Pictures [ Text { lineNo, string = describeKeyAndMapping key, emphasized }
           | (lineNo,key) <- zip [3..] keys
           , let emphasized = do
                   let action = keyMapping (key,Down)
                   case action of
                     TogglePause -> paused
                     Toggle but -> But.get but buttons
                     Drive but _ -> But.get but buttons
                     _ -> False
           ]
  where keys = [minBound..maxBound]

describeKeyAndMapping :: Key -> String
describeKeyAndMapping key = show key <> " : " <> show (keyMapping (key,Down))

instance Show Key where
  show = \case
    KeyEscape -> "[escape]"
    KeyDelete -> "[delete]"
    KeySpace -> "[space]"
    KeyLeft -> "[left]"
    KeyRight -> "[right]"
    KeyA -> "A"
    KeyS -> "S"
    KeyD -> "D"
    KeyW -> "W"

instance Show KeyAction where
  show = \case
    NoAction -> "NO-ACTION"
    Quit -> "Quit"
    TogglePause -> "Pause"
    ToggleControlDisplay -> "Show Keys"
    Drive but _ -> show but
    Toggle but -> show but


pictureState :: State -> Picture
pictureState s= Pictures
  [ Text { lineNo = 2, string = show s, emphasized = False }
  , pictureCanvas s
  ]

updateState :: Buttons -> State -> State
updateState b s =
  ( (if But.get But.Forwards b then State.forwards else id)
  . (if But.get But.Backwards b then State.backwards else id)
  . (if But.get But.TurnLeft b then State.turnLeft else id)
  . (if But.get But.TurnRight b then State.turnRight else id)
  . (if But.get But.StrafeLeft b then State.strafeLeft else id)
  . (if But.get But.StrafeRight b then State.strafeRight else id)
  ) s

pictureCanvas :: State -> Picture
pictureCanvas s = Pictures [ Pixel x y col | ((x,y),col) <- render s]
