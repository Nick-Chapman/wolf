
module Render
  ( canvasSize
  , State(..)
  , render
  ) where

import Prelude hiding (Int)
import Foreign.C.Types (CInt)
type Int = CInt

tmSize :: P2
tmSize = (8,8)

tileSize :: Int
tileSize = 10

canvasSize :: P2
canvasSize = scale tileSize tmSize

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

data State = STATE

render :: State -> [P2]
render STATE = do
  tilePos <- allTilePos
  let tile = tileAtPos tilePos
  if (tile == Off) then [] else do
    pix <- tilePoints tilePos
    pure pix

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


type P2 = (Int,Int)

scale :: Int -> P2 -> P2
scale n (x,y) = (n*x,n*y)

add :: P2 -> P2 -> P2
add (i,j) (x,y) = (i+x,j+y)
