
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
    | p `elem` [ (9,0), (10,0) ] -> Off -- hole in the outer wall
    | x == 0 || y == 0  || x == w-1 || y == h-1
      -> On
    | x >= 12 && x <= 13 && y >= 10 && y <= 11
      -> On
    | p `elem` [ (1,2), (2,4), (2,5), (3,5), (3,6) ]
      -> On
    | otherwise
      -> Off

render :: State -> [Pix]
render s =
  renderTiles
  -- ++ renderIntersections
  ++ renderPerson s
  ++ renderGaze s

renderGaze :: State -> [Pix]
renderGaze s = do
  --deg <- [-15,-10,-5,0,5,10,15]
  deg <- take 31 [-15,-14..]
  renderLooking (toAngle deg) s
  where
    toAngle :: Int -> Float
    toAngle deg = fromIntegral deg * 2 * pi / 360.0

renderLooking :: Angle -> State -> [Pix]
renderLooking angle s = do
  let (_miss,hit) = castRays angle s
  []
    ++ [ (trunc2 p, Magenta) | p <- _miss ]
    ++ case hit of Nothing -> []; Just p -> [ (trunc2 p, Green) ]

distanceSquared :: Point -> Point -> Float
distanceSquared (x1,y1) (x2,y2) =
  square (abs (x1 - x2))
  + square (abs (y1 - y2))
  where square n = n * n

trunc2 :: Point -> P2
trunc2 (x,y) = (truncate x, truncate y)

type Point = (Float,Float)

castRays :: Angle -> State -> ([Point],Maybe Point)
castRays angle s@State{px,py} = do
  let hpoints = take (fromIntegral n) $ castRaysH angle s where (n,_) = tmSize
  let vpoints = take (fromIntegral n) $ castRaysV angle s where (_,n) = tmSize
  let hchecked = [ (p, onTile p) | p <- hpoints ]
  let vchecked = [ (p, onTile p) | p <- vpoints ]
  let miss = [ p
             | (p,_) <-
               takeWhile (\(_,b) -> not b) hchecked
               ++ takeWhile (\(_,b) -> not b) vchecked
             ]
  let hit =
        case ( [ p | (p,b) <- hchecked, b ], [ p | (p,b) <- vchecked, b ] ) of
          ([],[]) -> Nothing
          ([],p:_) -> Just p
          (p:_,[]) -> Just p
          (p1:_,p2:_) -> do
            let d1 = distanceSquared p1 (px,py)
            let d2 = distanceSquared p2 (px,py)
            Just (if d1 < d2 then p1 else p2)
  (miss,hit)

onTile :: Point -> Bool
onTile (x,y) = do
  let tx = truncate x `div` tileSize
  let ty = truncate y `div` tileSize
  let tile = tileAtPos (tx,ty)
  (tile == On)

castRaysH :: Angle -> State -> [Point]
castRaysH angle State{px,py,pa} = do
  let a = angle + pa
  let lookingRight = cos a > 0
  let x0 :: Float = snapF px + (if lookingRight then 0 else -1)
  let dx :: Float = x0 - px
  i <- if lookingRight then [1..] else [0,-1..]
  let x :: Float = x0 + fromIntegral (i * tileSize)
  let y :: Float = py + tan a * (dx + fromIntegral (i * tileSize))
  pure (x,y)

castRaysV :: Angle -> State -> [Point]
castRaysV angle State{px,py,pa} = do
  let a = angle + pa
  let lookingUp = sin a < 0
  let y0 :: Float = snapF py + (if lookingUp then -1 else 0)
  let dy :: Float = y0 - py
  i :: Int <- if lookingUp then [0,-1..] else [1..]
  let y :: Float = y0 + fromIntegral (i * tileSize)
  let x :: Float = px + (1 / tan a) * (dy + fromIntegral (i * tileSize))
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
