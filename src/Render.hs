
module Render
  ( canvasSize
  , Colour(..)
  , State, state0
  , forwards, backwards, turnLeft, turnRight, strafeLeft, strafeRight
  , render
  , dump
  ) where

import Text.Printf (printf)
import Data.List (sortBy)
import Data.Ord (comparing)

import Prelude hiding (Int)
import Foreign.C.Types (CInt)
type Int = CInt

dump :: State -> IO ()
dump s = do
  print ("dump",s)

viewAngle :: Int
viewAngle = 60

canvasSize :: P2
canvasSize = (w+w,h) where (w,h) = planSize

tileSize :: Int
tileSize = 20

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
    | (x == 0 || x == w-1) && (y >= 0 && y <= h-1)
      -> On
    | (x >= 0 && x <= w-1) && (y == 0 || y == h-1)
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
  deriving Show

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
    printf "(%f,%f)@%f" px py pa

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
    stride = 2 * (fromIntegral tileSize / 16) * (30 / fps)
    fps = 20

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
    Just (p,side) -> do
      let d = sqrt (distanceSquared (px,py) p)
      let (_,h) = canvasSize
      let height = min (fromIntegral h * 0.9) (fromIntegral h * 5 / d)
      let hh = fromIntegral h / 2
      let y1 = truncate (hh - height)
      let y2 = truncate (hh + height)
      --let ys = [y1,y2] -- just top & bottom of wall
      let col = case side of N -> Green; S -> Green; E -> Blue; W -> Blue
      let ys = [y1..y2] -- just top & bottom of wall
      [ ((x,y), col) | y <- ys ]


gazeHeights :: State -> [(Int,Maybe (Point,Side))]
gazeHeights s = do
  let (w,_) = planSize
  i :: Int <- [0..w-1]
  let scale :: Float = fromIntegral w / fromIntegral viewAngle
  let c :: Int = i - (w `div` 2)
  let deg = fromIntegral c / scale
  let angle :: Float = toAngleF deg
  let (_,hit) = castRays angle s
  pure (i,hit)

toAngleF :: Float -> Float
toAngleF deg = deg * 2 * pi / 360.0

renderGaze :: Bool -> State -> [Pix]
renderGaze withMisses s = do
  i <- [0..viewAngle-1]
  let deg = i - (viewAngle `div` 2)
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
distanceSquared (x1,y1) (x2,y2) =
  square (abs (x1 - x2))
  + square (abs (y1 - y2))
  where square n = n * n

trunc2 :: Point -> P2
trunc2 (x,y) = (truncate x, truncate y)

type Point = (Float,Float)

castRays :: Angle -> State -> ([Point],Maybe (Point,Side))
castRays angle s@State{px,py} = do
  let hpoints = take (fromIntegral n) $ castRaysH angle s where (n,_) = tmSize
  let vpoints = take (fromIntegral n) $ castRaysV angle s where (_,n) = tmSize
  let points = hpoints ++ vpoints
  let checked = [ (p,side, distanceSquared (px,py) p, onTile p side)
                | (p,side) <- points
                ]
  let ordered = sortBy (comparing (\(_,_,d,_) -> d)) checked -- TODO: terrible
  let miss = [ p | (p,_,_,_) <- takeWhile (\(_,_,_,b) -> not b) ordered]
  let hit = case [ (p,side)
                 | (p,side,_,b) <- ordered, b ] of [] -> Nothing; x:_ -> Just x
  (miss,hit)

data Side = N | E | S | W

onTile :: Point -> Side -> Bool
onTile (x,y) side = do
  let x' = case side of E -> x-1; _ -> x
  let y' = case side of S -> y-1; _ -> y
  let tx = truncate x' `div` tileSize
  let ty = truncate y' `div` tileSize
  let tile = tileAtPos (tx,ty)
  (tile == On)

castRaysH :: Angle -> State -> [(Point,Side)]
castRaysH angle State{px,py,pa} = do
  let a = angle + pa
  let lookingRight = cos a > 0
  let side = if lookingRight then W else E
  let x0 :: Float = snapF px
  let dx :: Float = x0 - px
  i <- if lookingRight then [1..] else [0,-1..]
  let x :: Float = x0 + fromIntegral (i * tileSize)
  let y :: Float = py + tan a * (dx + fromIntegral (i * tileSize))
  pure ((x,y),side)

castRaysV :: Angle -> State -> [(Point,Side)]
castRaysV angle State{px,py,pa} = do
  let a = angle + pa
  let lookingDown = sin a > 0
  let side = if lookingDown then N else S
  let y0 :: Float = snapF py
  let dy :: Float = y0 - py
  i :: Int <- if lookingDown then [1..] else [0,-1..]
  let y :: Float = y0 + fromIntegral (i * tileSize)
  let x :: Float = px + (1 / tan a) * (dy + fromIntegral (i * tileSize))
  pure ((x,y),side)

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
