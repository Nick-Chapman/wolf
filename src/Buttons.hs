
module Buttons
  ( Buttons, buttons0
  , But(..), get, set, press, release, toggle,
  ) where

import Data.Map (Map)
import qualified Data.Map.Strict as Map

data But
  = Forwards
  | Backwards
  | TurnLeft
  | TurnRight
  | StrafeLeft
  | StrafeRight
  deriving (Eq,Ord,Show)

newtype Buttons = Buttons { map :: Map But Bool } deriving Show

buttons0 :: Buttons
buttons0 = Buttons { map = Map.empty }

get :: But -> Buttons -> Bool
get but Buttons{map} = Map.findWithDefault False but map

set :: Bool -> But -> Buttons -> Buttons
set v but Buttons{map} = Buttons { map = Map.insert but v map }

press :: But -> Buttons -> Buttons
press = set True

release :: But -> Buttons -> Buttons
release = set False

toggle :: But -> Buttons -> Buttons
toggle b buttons = set (not (get b buttons)) b buttons
