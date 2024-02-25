module Top (main) where

import System.Environment (getArgs)
import qualified GameLoop (main,Conf(..))

main :: IO ()
main = do
  putStrLn "*wolf*"
  args <- getArgs
  let Conf{fps,sf} = parse args conf0
  GameLoop.main $ GameLoop.Conf { sf0 = sf, fps0 = fps }

parse :: [String] -> Conf -> Conf
parse args conf = case args of
  [] -> conf
  "-sf":i:args -> parse args $ conf { sf = read i }
  "-fps":i:args -> parse args $ conf { fps = read i }
  args ->
    error $ "parseArgs: " ++ show args

data Conf = Conf
  { fps :: Int
  , sf :: Int
  }

conf0 :: Conf
conf0 = Conf
  { fps = 20
  , sf = 2
  }
