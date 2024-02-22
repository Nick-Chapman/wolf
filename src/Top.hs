module Top (main) where

import System.Environment (getArgs)
import qualified GraphicsSDL as SDL (main,Conf(..))

main :: IO ()
main = do
  putStrLn "*wolf*"
  args <- getArgs
  let Conf{mode,fpsLimit,scaleFactor} = parse args conf0
  case mode of
    ModePlay -> do
      SDL.main $ SDL.Conf { scaleFactor, fpsLimit, showControls = True }

parse :: [String] -> Conf -> Conf
parse args conf = case args of
  [] -> conf
  "-sf":i:args -> parse args $ conf { scaleFactor = read i }
  "-fps":i:args -> parse args $ conf { fpsLimit = Just (read i) }
  args ->
    error $ "parseArgs: " ++ show args

data Mode
  = ModePlay

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
