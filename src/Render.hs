
module Render
  ( canvasSize
  , Pix, Colour(..)
  , render
  ) where

import Data.List (sortBy)
import Data.Ord (comparing)
import State (State(..),planSize)
import Prelude hiding (Int)

import Foreign.C.Types (CInt)
type Int = CInt

type P2 = (Int,Int)

scale :: Int -> P2 -> P2
scale n (x,y) = (n*x,n*y)

canvasSize :: State -> P2
canvasSize s = (w+w,h) where (w,h) = planSize s

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

add :: P2 -> P2 -> P2
add (i,j) (x,y) = (i+x,j+y)

data Colour
  = Black | White | Red | Blue | Green | Yellow | DarkGrey | LightGrey | Magenta
  | DarkGreen | DarkBlue
  deriving Show

type Pix = (P2,Colour)

render :: State -> [Pix]
render s =
  let off = (w,0) where (w,_) = planSize s in
  renderTiles s
  ++ renderPerson s
  ++ renderGaze True s
  ++ [ (p `add` off,c) | (p,c) <- renderWalls s ]

renderWalls :: State -> [Pix]
renderWalls s@State{px,py,heightScale} = do
  (x,popt) <- gazeHeights s
  case popt of
    Nothing -> []
    Just (p@(xHit,yHit),side) -> do
      let d = sqrt (distanceSquared (px,py) p)
      let (_,hCanvas) = canvasSize s
      let height = min (fromIntegral hCanvas * 0.9) (fromIntegral (hCanvas * heightScale) / d)
      --let hh = fromIntegral hCanvas / 2

      let fracComp f = f - fromIntegral (floor f :: Int)
      let onNS = case side of N -> True; S -> True; E -> False; W -> False
      let xTex = fracComp ((if onNS then xHit else yHit) / fromIntegral (tileSize s))
      let
        vcol y = do
          let yTex = fromIntegral y / height
          let texture = theTexture (xTex, yTex)
          let green = if texture then DarkGreen else Green
          let blue = if texture then DarkBlue else Blue
          if onNS then green else blue

      let base = truncate ((fromIntegral hCanvas - height) / 2)
      [ ((x,y), vcol i)
        | i <- [0..truncate height-1]
        , let y = base + i
        ]

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

type Angle = Float

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

----------------------------------------------------------------------
-- textures

theTexture :: Point -> Bool
theTexture (x,y) = do
  assertInUnitRange "x" x $ do
  assertInUnitRange "y" y $ do
  let x1 :: Int = truncate (10*x) -- 0..9
  let y1 = truncate (10*y) -- 0..9
  (x1+y1) `mod` 2 == 0

assertInUnitRange :: String -> Float -> a -> a
assertInUnitRange tag x a =
  if x>=0 && x < 1 then a else error (show ("assertInUnitRange",tag,x))
