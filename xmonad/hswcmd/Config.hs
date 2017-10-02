
module Config
  (
    config
  )
  where

import qualified Data.Map as M

import System.Wacom.Config

config :: Config
config =
  dfltConfig {
    tMapAreas = ["1920x1200+0+0", "1920x1080+1920+0"],
    tProfiles = buildProfiles [Profile "Default" [ringScrolls, ringBrackets] buttons]
  }

ringScrolls :: RingMode
ringScrolls = RingMode "Scroll" (Click 5) (Click 4)

ringBrackets :: RingMode
ringBrackets = RingMode "Brackets" (Key "[") (Key "]")

buttons :: ButtonsMap
buttons = ButtonsMap $ M.fromList [
           (2, Key "ctrl"),
           (3, Key "shift"),
           (13, Key "ctrl z"),
           (12, Key "Delete")
          ]

