module Top (main) where

import qualified GameLoop (initWorld,main)

main :: IO ()
main = do
  let world = GameLoop.initWorld
  GameLoop.main world
