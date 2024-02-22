
module Render
  ( canvasSize
  , Colour(..)
  , State, state0
  , forwards, backwards, turnLeft, turnRight, strafeLeft, strafeRight
  , render
  ) where

import Prelude hiding (Int)
import Foreign.C.Types (CInt)
type Int = CInt

tmSize :: P2
tmSize = (8,8)

tileSize :: Int
tileSize = 16

type P2 = (Int,Int)

scale :: Int -> P2 -> P2
scale n (x,y) = (n*x,n*y)

add :: P2 -> P2 -> P2
add (i,j) (x,y) = (i+x,j+y)

data Colour = Black | White | Red | Blue | Green | Yellow | DarkGrey | LightGrey

type Pix = (P2,Colour)

canvasSize :: P2
canvasSize = scale tileSize tmSize

wrapCanvas :: P2 -> P2
wrapCanvas (i,j) = ((i+w) `mod` w, (j+h) `mod` h)
  where (w,h) = canvasSize

data State = State
  { px :: Float
  , py :: Float
  , pa :: Angle
  } deriving Show

type Angle = Float

state0 :: State
state0 = State
  { px = fromIntegral (w `div` 2)
  , py = fromIntegral (h `div` 2)
  , pa = 0
  }
  where (w,h) = canvasSize

forwards,backwards,turnLeft,turnRight,strafeLeft,strafeRight :: State -> State

(turnLeft,turnRight) = (left,right)
  where
    left s@State{pa} = s { pa = pa - angularTurnPerFrame }
    right s@State{pa} = s { pa = pa + angularTurnPerFrame }
    angularTurnPerFrame = 0.05

(forwards,backwards,strafeLeft,strafeRight) = (fore,back,left,right)
  where
    fore s@State{px,py,pa} =
      s { px = px + cos pa * stride
        , py = py + sin pa * stride
        }
    back s@State{px,py,pa} =
      s { px = px - cos pa * stride
        , py = py - sin pa * stride
        }
    left s@State{px,py,pa} =
      s { px = px + sin pa * stride
        , py = py - cos pa * stride
        }
    right s@State{px,py,pa} =
      s { px = px - sin pa * stride
        , py = py + cos pa * stride
        }
    stride = 0.5

data Tile = On | Off deriving (Eq)

tileAtPos :: P2 -> Tile
tileAtPos p@(x,y) = do
  let (w,h) = tmSize
  if
    | p `elem` [ (1,2), (2,4) ]
      -> On
    | x == 0 || y == 0  || x == w-1 || y == h-1
      -> On
    | otherwise
      -> Off

render :: State -> [Pix]
render s = renderTiles ++ renderPerson s

renderPerson :: State -> [Pix]
renderPerson s =
  [ (wrapCanvas (personPos s),Red)
  , (wrapCanvas (nosePos s),Blue)
  ]

personPos :: State -> P2
personPos State{px,py} = (truncate px,truncate py)

nosePos :: State -> P2
nosePos State{px,py,pa} = (truncate nx,truncate ny)
  where
    nx = px + dx
    ny = py + dy
    dx = noseLen * cos pa
    dy = noseLen * sin pa
    noseLen = 10

renderTiles :: [Pix]
renderTiles = do
  tilePos <- allTilePos
  let tile = tileAtPos tilePos
  if (tile == Off) then [] else do
    pos <- tilePoints tilePos
    pure (pos,Yellow)

allTilePos :: [P2]
allTilePos = do
  let (i,j) = tmSize
  x <- [0..i-1]
  y <- [0..j-1]
  pure (x,y)

tilePoints :: P2 -> [P2]
tilePoints pos = do
  let n = tileSize
  xo <- [1..n-2]
  yo <- [1..n-2]
  let off = (xo,yo)
  pure (scale n pos `add` off)
