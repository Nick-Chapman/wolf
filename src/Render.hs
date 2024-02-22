
module Render
  ( canvasSize
  , Colour(..)
  , State, state0
  , forwards, backwards, turnLeft, turnRight, strafeLeft, strafeRight
  , render
  ) where

import Text.Printf (printf)

import Prelude hiding (Int)
import Foreign.C.Types (CInt)
type Int = CInt

tmSize :: P2
tmSize = (20,15)

tileSize :: Int
tileSize = 16

type P2 = (Int,Int)

scale :: Int -> P2 -> P2
scale n (x,y) = (n*x,n*y)

add :: P2 -> P2 -> P2
add (i,j) (x,y) = (i+x,j+y)

data Colour = Black | White | Red | Blue | Green | Yellow | DarkGrey | LightGrey | Magenta

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
  }

instance Show State where
  show State{px,py,pa} =
    printf "(%f,%f)@%f [%s]" px py pa (show r)
    where r = cos pa > 0

type Angle = Float

state0 :: State
state0 = State
  { px = fromIntegral ((w+tileSize) `div` 2)
  , py = fromIntegral ((h+tileSize) `div` 2)
  , pa = 0
  }
  where (w,h) = canvasSize

forwards,backwards,turnLeft,turnRight,strafeLeft,strafeRight :: State -> State

(turnLeft,turnRight) = (left,right)
  where
    left s@State{pa} = s { pa = pa - angularTurnPerFrame }
    right s@State{pa} = s { pa = pa + angularTurnPerFrame }
    angularTurnPerFrame = 0.1 -- TODO: should be scaled via fps

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
    stride = 2 -- TODO: should be scaled via fps

data Tile = On | Off deriving (Eq)

tileAtPos :: P2 -> Tile
tileAtPos p@(x,y) = do
  let (w,h) = tmSize
  if
    | p `elem` [ (1,2), (2,4), (2,5), (3,5), (3,6) ]
      -> On
    | x == 0 || y == 0  || x == w-1 || y == h-1
      -> On
    | otherwise
      -> Off

render :: State -> [Pix]
render s =
  renderTiles
  -- ++ renderIntersections
  ++ renderPerson s
  ++ renderLooking s


renderLooking :: State -> [Pix]
renderLooking s = do
  (x,y) <- castRaysH s ++ castRaysV s
  let tx = truncate x `div` tileSize
  let ty = truncate y `div` tileSize
  let tile = tileAtPos (tx,ty)
  let hit = tile == On
  let col = if hit then Green else Magenta
  let pos = (truncate x,truncate y)
  pure (pos,col)

type Point = (Float,Float)

castRaysH :: State -> [Point]
castRaysH State{px,py,pa} = do
  let (maxSteps,_) = tmSize
  let lookingRight = cos pa > 0
  let x0 :: Float = snapF px + (if lookingRight then 0 else -1)
  let dx :: Float = x0 - px
  i <- take (fromIntegral maxSteps) (if lookingRight then [1..] else [0,-1..])
  let x :: Float = x0 + fromIntegral (i * tileSize)
  let y :: Float = py + tan pa * (dx + fromIntegral (i * tileSize))
  pure (x,y)


castRaysV :: State -> [Point]
castRaysV State{px,py,pa} = do
  let (_,maxSteps) = tmSize
  let lookingUp = sin pa < 0
  let y0 :: Float = snapF py + (if lookingUp then -1 else 0)
  let dy :: Float = y0 - py
  i :: Int <- take (fromIntegral maxSteps) (if lookingUp then [0,-1..] else [1..])
  let y :: Float = y0 + fromIntegral (i * tileSize)
  let x :: Float = px + (1 / tan pa) * (dy + fromIntegral (i * tileSize))
  pure (x,y)


snapF :: Float -> Float
snapF i = fromIntegral ((truncate i `div` tileSize) * tileSize)


renderPerson :: State -> [Pix]
renderPerson s =
  [ (wrapCanvas (personPos s),Red)
--  , (wrapCanvas (nosePos s),Blue)
  ]

personPos :: State -> P2
personPos State{px,py} = (truncate px,truncate py)

{-nosePos :: State -> P2
nosePos State{px,py,pa} = (truncate nx,truncate ny)
  where
    nx = px + dx
    ny = py + dy
    dx = noseLen * cos pa
    dy = noseLen * sin pa
    noseLen = 3-}


{-renderIntersections :: [Pix]
renderIntersections = do
  let n = tileSize
  pos <- allTilePos
  xo <- [0,n-1]
  yo <- [0,n-1]
  let off = (xo,yo)
  pure ((scale n pos `add` off),LightGrey)-}


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
