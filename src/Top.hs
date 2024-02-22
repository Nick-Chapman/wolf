module Top (main) where

import System.Environment (getArgs)
import qualified GraphicsSDL as SDL (main,Conf(..))
import qualified Render

main :: IO ()
main = do
  putStrLn "*wolf*"
  args <- getArgs
  let Conf{mode,fpsLimit,scaleFactor} = parse args conf0
  case mode of
    ModePlay -> do
      SDL.main $ SDL.Conf { scaleFactor, fpsLimit, showControls = False }
    ModeExplore -> do
      testRender

parse :: [String] -> Conf -> Conf
parse args conf = case args of
  [] -> conf
  "-sf":i:args -> parse args $ conf { scaleFactor = read i }
  "-fps":i:args -> parse args $ conf { fpsLimit = Just (read i) }
  args ->
    error $ "parseArgs: " ++ show args


testRender :: IO ()
testRender = do
  print "*render*"
  let s0 = Render.STATE
  --print Render.canvasSize
  let xs = Render.render s0
  mapM_ print xs


data Mode
  = ModePlay
  | ModeExplore

data Conf = Conf
  { mode :: Mode
  , fpsLimit :: Maybe Int
  , scaleFactor :: Int
  }

conf0 :: Conf
conf0 = Conf
  { mode = ModePlay
  , fpsLimit = Just 60 --Nothing
  , scaleFactor = 4
  }
