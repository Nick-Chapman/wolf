
module State
  ( State(..), state0
  , planSize
  , forwards, backwards, turnLeft, turnRight, strafeLeft, strafeRight
  , incAA, decAA, selectAA
  ) where

import Prelude hiding (Int)
import Foreign.C.Types (CInt)
type Int = CInt

data State = State
  { aa :: AdjustableAttribute
  , fps :: Int
  , heightScale :: Int
  , viewAngle :: Int
  , tileSize :: Int
  , tmSize :: Int
  , px :: Float
  , py :: Float
  , pa :: Float
  } deriving Show

state0 :: State
state0 = s
  where
    s = State
      { aa = FPS
      , fps = 20
      , heightScale = 20
      , viewAngle = 60
      , tileSize = 20
      , tmSize = 15
      , px = fromIntegral ((w + tileSize s) `div` 2)
      , py = fromIntegral ((h + tileSize s) `div` 2)
      , pa = - (pi / 2)
      }
    (w,h) = planSize s

type P2 = (Int,Int)

planSize :: State -> P2
planSize s = scale (tileSize s) (n,n) where n = tmSize s

scale :: Int -> P2 -> P2
scale n (x,y) = (n*x,n*y)

data AdjustableAttribute = FPS | HeightScale | ViewAngle | TileSize
  deriving Show

incAA :: State -> State
incAA s@State{aa,tileSize,heightScale,viewAngle,fps} =
  case aa of
    FPS -> s { fps = min 60 (fps + 1) }
    HeightScale -> s { heightScale = heightScale + 1 }
    ViewAngle -> s { viewAngle = viewAngle + 1 }
    TileSize -> s { tileSize = tileSize + 1 }

decAA :: State -> State
decAA s@State{aa,tileSize,heightScale,viewAngle,fps} =
  case aa of
    FPS -> s { fps = max 1 (fps - 1) }
    HeightScale -> s { heightScale = max 1 (heightScale - 1) }
    ViewAngle -> s { viewAngle = max 1 (viewAngle - 1) }
    TileSize -> s { tileSize = max 1 (tileSize - 1) }

selectAA :: State -> State
selectAA s@State{aa} =
  case aa of
    FPS -> s { aa = HeightScale }
    HeightScale -> s { aa = ViewAngle }
    ViewAngle -> s { aa = TileSize }
    TileSize -> s { aa = FPS }

forwards,backwards,turnLeft,turnRight,strafeLeft,strafeRight :: State -> State

(turnLeft,turnRight) = (left,right)
  where
    left s@State{pa} = s { pa = pa - angularTurnPerFrame s }
    right s@State{pa} = s { pa = pa + angularTurnPerFrame s }
    angularTurnPerFrame s = 0.1 * 20 / fromIntegral (fps s)

(forwards,backwards,strafeLeft,strafeRight) = (fore,back,left,right)
  where
    fore s@State{px,py,pa} =
      s { px = px + cos pa * stride s
        , py = py + sin pa * stride s
        }
    back s@State{px,py,pa} =
      s { px = px - cos pa * stride s
        , py = py - sin pa * stride s
        }
    left s@State{px,py,pa} =
      s { px = px + sin pa * stride s
        , py = py - cos pa * stride s
        }
    right s@State{px,py,pa} =
      s { px = px - sin pa * stride s
        , py = py + cos pa * stride s
        }
    stride s = 2 * (fromIntegral (tileSize s) / 16) * (30 / fromIntegral (fps s))
