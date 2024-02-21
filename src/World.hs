
module World
  ( World, initWorld, stepFrame
  , Key(..), KeyMotion(..), updateKey
  , Picture(..), pictureWorld
  ) where

import Buttons (Buttons,buttons0,But)
import qualified Buttons as But (get,press,release,toggle,But(..))

data Key
  = KeyEscape
  | KeyDelete
  | KeySpace
  | KeyA
  | KeyS
  | KeyD
  | KeyW
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
  (KeyA,m) -> Drive But.TurnLeft m
  (KeyS,m) -> Drive But.Backwards m
  (KeyD,m) -> Drive But.TurnRight m
  (KeyW,m) -> Drive But.Forwards m
  _ -> NoAction

data Picture
  = Pictures [Picture]
  | Text { string :: String, lineNo :: Int, emphasized :: Bool }
  | Pixel { x :: Int, y :: Int }

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
           | (lineNo,key) <- zip [5..] keys
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
    KeyA -> "A"
    KeyS -> "S"
    KeyD -> "D"
    KeyW -> "W"

instance Show KeyAction where
  show = \case
    NoAction -> "NO-ACTION"
    Quit -> "QUIT"
    TogglePause -> "PAUSE"
    ToggleControlDisplay -> "SHOW-KEYS"
    Drive but _ -> show but
    Toggle but -> show but


data State = State
  { px :: Int
  , py :: Int
  }

state0 :: State
state0 = State
  { px = 20
  , py = 30
  }

pictureState :: State -> Picture
pictureState State{px,py} = Pictures
  [ Text { lineNo = 2, string = "px : " <> show px, emphasized = False }
  , Text { lineNo = 3, string = "py : " <> show py, emphasized = False }
  , pictureCanvas
  ]


updateState :: Buttons -> State -> State
updateState b s@State{px,py} = do
  s { px =
      ((\px -> if But.get But.Forwards b then px + 1 else px)
       . (\px -> if But.get But.Backwards b then px - 1 else px)
      ) px
    , py =
      ((\py -> if But.get But.TurnLeft b then py + 1 else py)
       . (\py -> if But.get But.TurnRight b then py - 1 else py)
      ) py
    }


pictureCanvas :: Picture
pictureCanvas = do
  Pictures
    [ Pixel x y
    | x <- [0..255]
    , y <- [0..255]
    , let on = (x `div` 4 + y `div` 4) `mod` 2 == (0::Int)
    , on
    ]
