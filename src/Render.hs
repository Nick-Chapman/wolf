
module Render
  ( canvasSize
  , Pix, Colour(..)
  , State, state0
  , forwards, backwards, turnLeft, turnRight, strafeLeft, strafeRight
  , render
  ) where

import Data.List (sortBy)
import Data.Ord (comparing)

import Prelude hiding (Int)
import Foreign.C.Types (CInt)
type Int = CInt

data State = State
  { px :: Float
  , py :: Float
  , pa :: Angle
  , tileSize :: Int
  , viewAngle :: Int
  , tmSize :: Int
  } deriving Show

canvasSize :: State -> P2
canvasSize s = (w+w,h) where (w,h) = planSize s

planSize :: State -> P2
planSize s = scale (tileSize s) (n,n) where n = tmSize s

data Tile = On | Off deriving (Eq)

tileAtPos :: State -> P2 -> Tile
tileAtPos s p@(x,y) = do
  let n = tmSize s
  let (w,h) = (n,n)
  if
    | p `elem` [ (9,0), (10,0) ] -> Off -- hole in the outer wall
    | (x == 0 || x == w-1) && (y >= 0 && y <= h-1)
      -> On
    | (x >= 0 && x <= w-1) && (y == 0 || y == h-1)
      -> On
    | p `elem` [ (5,6), (6,5), (7,4), (8,4), (9,5), (10,6) ]
      -> On
    | otherwise
      -> Off

type P2 = (Int,Int)

scale :: Int -> P2 -> P2
scale n (x,y) = (n*x,n*y)

add :: P2 -> P2 -> P2
add (i,j) (x,y) = (i+x,j+y)

data Colour = Black | White | Red | Blue | Green | Yellow | DarkGrey | LightGrey | Magenta
  deriving Show

type Pix = (P2,Colour)

type Angle = Float

state0 :: State
state0 = s
  where
    s = State
      { px = fromIntegral ((w + tileSize s) `div` 2)
      , py = fromIntegral ((h + tileSize s) `div` 2)
      , pa = - (pi / 2)
      , tileSize = 20
      , viewAngle = 60
      , tmSize = 15
      }
    (w,h) = planSize s

forwards,backwards,turnLeft,turnRight,strafeLeft,strafeRight :: State -> State

(turnLeft,turnRight) = (left,right)
  where
    left s@State{pa} = s { pa = pa - angularTurnPerFrame }
    right s@State{pa} = s { pa = pa + angularTurnPerFrame }
    angularTurnPerFrame = 0.1 -- TODO: should be scaled via fps

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
    stride s = 2 * (fromIntegral (tileSize s) / 16) * (30 / fps)
    fps = 20

render :: State -> [Pix]
render s =
  let off = (w,0) where (w,_) = planSize s in
  renderTiles s
  ++ renderPerson s
  ++ renderGaze True s
  ++ [ (p `add` off,c) | (p,c) <- renderWalls s ]

renderWalls :: State -> [Pix]
renderWalls s@State{px,py} = do
  (x,popt) <- gazeHeights s
  case popt of
    Nothing -> []
    Just (p,side) -> do
      let d = sqrt (distanceSquared (px,py) p)
      let (_,h) = canvasSize s
      let height = min (fromIntegral h * 0.9) (fromIntegral h * 5 / d)
      let hh = fromIntegral h / 2
      let y1 = truncate (hh - height)
      let y2 = truncate (hh + height)
      let col = case side of N -> Green; S -> Green; E -> Blue; W -> Blue
      [ ((x,y), col) | y <- [y1..y2] ]

gazeHeights :: State -> [(Int,Maybe (Point,Side))]
gazeHeights s = do
  let (w,_) = planSize s
  i :: Int <- [0..w-1]
  let scale :: Float = fromIntegral w / fromIntegral (viewAngle s)
  let c :: Int = i - (w `div` 2)
  let deg = fromIntegral c / scale
  let angle :: Float = toAngleF deg
  let (_,hit) = castRays angle s
  pure (i,hit)

toAngleF :: Float -> Float
toAngleF deg = deg * 2 * pi / 360.0

renderGaze :: Bool -> State -> [Pix]
renderGaze withMisses s = do
  i <- [0..viewAngle s - 1]
  let deg = i - (viewAngle s `div` 2)
  renderLooking withMisses (toAngle deg) s

toAngle :: Int -> Float
toAngle deg = fromIntegral deg * 2 * pi / 360.0

renderLooking :: Bool -> Angle -> State -> [Pix]
renderLooking withMisses angle s = do
  let (miss,hit) = castRays angle s
  []
    ++ (if withMisses then [ ((trunc2 p), Magenta) | p <- miss ] else [])
    ++ case hit of Nothing -> []; Just (p,_) -> [ ((trunc2 p), Green) ]

distanceSquared :: Point -> Point -> Float
distanceSquared (x1,y1) (x2,y2) = square (abs (x1 - x2)) + square (abs (y1 - y2))
  where square n = n * n

trunc2 :: Point -> P2
trunc2 (x,y) = (truncate x, truncate y)

type Point = (Float,Float)

castRays :: Angle -> State -> ([Point],Maybe (Point,Side))
castRays angle s@State{px,py} = do
  let n = tmSize s
  let hpoints = take (fromIntegral n) $ castRaysH angle s
  let vpoints = take (fromIntegral n) $ castRaysV angle s
  let points = hpoints ++ vpoints
  let checked = [ (p,side, distanceSquared (px,py) p, onTile s p side)
                | (p,side) <- points
                ]
  let ordered = sortBy (comparing (\(_,_,d,_) -> d)) checked -- TODO: terrible
  let miss = [ p | (p,_,_,_) <- takeWhile (\(_,_,_,b) -> not b) ordered]
  let hit = case [ (p,side)
                 | (p,side,_,b) <- ordered, b ] of [] -> Nothing; x:_ -> Just x
  (miss,hit)

data Side = N | E | S | W

onTile :: State -> Point -> Side -> Bool
onTile s (x,y) side = do
  let x' = case side of E -> x-1; _ -> x
  let y' = case side of S -> y-1; _ -> y
  let tx = truncate x' `div` tileSize s
  let ty = truncate y' `div` tileSize s
  let tile = tileAtPos s (tx,ty)
  (tile == On)

castRaysH :: Angle -> State -> [(Point,Side)]
castRaysH angle s@State{px,py,pa} = do
  let a = angle + pa
  let lookingRight = cos a > 0
  let side = if lookingRight then W else E
  let x0 :: Float = snapF s px
  let dx :: Float = x0 - px
  i <- if lookingRight then [1..] else [0,-1..]
  let x :: Float = x0 + fromIntegral (i * tileSize s)
  let y :: Float = py + tan a * (dx + fromIntegral (i * tileSize s))
  pure ((x,y),side)

castRaysV :: Angle -> State -> [(Point,Side)]
castRaysV angle s@State{px,py,pa} = do
  let a = angle + pa
  let lookingDown = sin a > 0
  let side = if lookingDown then N else S
  let y0 :: Float = snapF s py
  let dy :: Float = y0 - py
  i :: Int <- if lookingDown then [1..] else [0,-1..]
  let y :: Float = y0 + fromIntegral (i * tileSize s)
  let x :: Float = px + (1 / tan a) * (dy + fromIntegral (i * tileSize s))
  pure ((x,y),side)

snapF :: State -> Float -> Float
snapF s i = fromIntegral ((truncate i `div` tileSize s) * tileSize s)

renderPerson :: State -> [Pix]
renderPerson s = [ (wrapPlan s (personPos s),Red) ]

wrapPlan :: State -> P2 -> P2
wrapPlan s (i,j) = ((i+w) `mod` w, (j+h) `mod` h)
  where (w,h) = planSize s

personPos :: State -> P2
personPos State{px,py} = (truncate px,truncate py)

renderTiles :: State -> [Pix]
renderTiles s = do
  tilePos <- allTilePos s
  let tile = tileAtPos s tilePos
  if (tile == Off) then [] else do
    pos <- tilePoints s tilePos
    pure (pos,Yellow)

allTilePos :: State -> [P2]
allTilePos s = do
  let n = tmSize s
  x <- [0..n-1]
  y <- [0..n-1]
  pure (x,y)

tilePoints :: State -> P2 -> [P2]
tilePoints s pos = do
  let n = tileSize s
  xo <- [1..n-2]
  yo <- [1..n-2]
  let off = (xo,yo)
  pure (scale n pos `add` off)
