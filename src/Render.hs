
module Render
  ( canvasSize
  , Colour(..)
  , State, state0
  , forwards, backwards, turnLeft, turnRight, strafeLeft, strafeRight
  , render
  ) where

import Text.Printf (printf)

import Data.List (sortBy)
import Data.Ord (comparing)


import Prelude hiding (Int)
import Foreign.C.Types (CInt)
type Int = CInt

canvasSize :: P2
canvasSize = (w+w,h) where (w,h) = planSize

tileSize :: Int
tileSize = 16

tmSize :: P2
tmSize = (15,15)

planSize :: P2
planSize = scale tileSize tmSize

data Tile = On | Off deriving (Eq)

tileAtPos :: P2 -> Tile
tileAtPos p@(x,y) = do
  let (w,h) = tmSize
  if
    | p `elem` [ (9,0), (10,0) ] -> Off -- hole in the outer wall
    | x == 0 || y == 0  || x == w-1 || y == h-1
      -> On
    | x >= 10 && x <= 11 && y >= 10 && y <= 11
      -> On
    | p `elem` [ (1,2), (2,4), (2,5), (3,5), (3,6) ]
      -> On
    | otherwise
      -> Off

type P2 = (Int,Int)

scale :: Int -> P2 -> P2
scale n (x,y) = (n*x,n*y)

add :: P2 -> P2 -> P2
add (i,j) (x,y) = (i+x,j+y)

data Colour = Black | White | Red | Blue | Green | Yellow | DarkGrey | LightGrey | Magenta

type Pix = (P2,Colour)

wrapPlan :: P2 -> P2
wrapPlan (i,j) = ((i+w) `mod` w, (j+h) `mod` h)
  where (w,h) = planSize

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
  where (w,h) = planSize

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

render :: State -> [Pix]
render s =
  let off = (w,0) where (w,_) = planSize in
  renderTiles
  ++ renderPerson s
  ++ renderGaze True s
  ++ [ (p `add` off,c) | (p,c) <- renderWalls s ]

{-renderWalls :: State -> [Pix]
renderWalls s = do
  (_,popt) <- gazeHeights s
  case popt of
    Nothing -> []
    Just p -> [ (trunc2 p, Green) ]-}

renderWalls :: State -> [Pix]
renderWalls s@State{px,py} = do
  (x,popt) <- gazeHeights s
  case popt of
    Nothing -> []
    Just p -> do
      let d = sqrt (distanceSquared (px,py) p)
      let (_,h) = planSize
      let height = min 500 (1000 / d)
      let hh = fromIntegral h / 2
      let y1 = truncate (hh - height)
      let y2 = truncate (hh + height)
      [ ((x,y1), Green), ((x,y2), Green)]

gazeHeights :: State -> [(Int,Maybe Point)]
gazeHeights s = do
  let viewAngle :: Float = 30.0
  let (w,_) = planSize
  i :: Int <- [0..w-1]
  let scale :: Float = fromIntegral w / viewAngle
  let c :: Int = i - (w `div` 2)
  let deg = fromIntegral c / scale
  let angle :: Float = toAngleF deg
  let (_,hit) = castRays angle s
  pure (i,hit)

toAngleF :: Float -> Float
toAngleF deg = deg * 2 * pi / 360.0

renderGaze :: Bool -> State -> [Pix]
renderGaze withMisses s = do
  deg <- take 31 [-15,-14..]
  renderLooking withMisses (toAngle deg) s

toAngle :: Int -> Float
toAngle deg = fromIntegral deg * 2 * pi / 360.0

renderLooking :: Bool -> Angle -> State -> [Pix]
renderLooking withMisses angle s = do
  let (miss,hit) = castRays angle s
  []
    ++ (if withMisses then [ ((trunc2 p), Magenta) | p <- miss ] else [])
    ++ case hit of Nothing -> []; Just p -> [ ((trunc2 p), Green) ]

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
  let points = hpoints ++ vpoints
  let checked = [ (p, distanceSquared (px,py) p, onTile p) | p <- points ]
  let ordered = sortBy (comparing (\(_,d,_) -> d)) checked
  let miss = [ p | (p,_,_) <- takeWhile (\(_,_,b) -> not b) ordered]
  let hit = case [ p | (p,_,b) <- ordered, b ] of [] -> Nothing; p:_ -> Just p
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
  [ (wrapPlan (personPos s),Red)
  ]

personPos :: State -> P2
personPos State{px,py} = (truncate px,truncate py)

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
