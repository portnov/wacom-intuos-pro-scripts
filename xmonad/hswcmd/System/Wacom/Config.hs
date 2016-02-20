
module System.Wacom.Config
  (
    Config (..),
    Profile (..),
    RingMode (..),
    TabletAction (..),
    Area,
    dfltConfig,
    buildProfiles
  )
  where

import qualified Data.Map as M

data Config = Config {
    tStylusKey :: String,
    tPadKey :: String,
    tTouchKey :: String,
    tTouch :: Bool,
    tMapAreas :: [Area],
    tProfiles :: [(String, Profile)]
  }
  deriving (Show)

type Area = String

data Profile = Profile {
    pName :: String,
    pRing :: [RingMode],
    pButtons :: M.Map Int TabletAction
  }
  deriving (Show)

data RingMode = RingMode {
    rName :: String,
    ringUp :: TabletAction,
    ringDown :: TabletAction
  }
  deriving (Show)

data TabletAction =
    Key String
  | Click Int
  | DblClick Int

instance Show TabletAction where
  show (Key s) = "key " ++ s
  show (Click b) = "button " ++ show b
  show (DblClick b) = "dblclick " ++ show b

dfltConfig :: Config
dfltConfig = Config {
  tStylusKey = "Pen",
  tPadKey = "Pad",
  tTouchKey = "Finger",
  tMapAreas = [],
  tTouch = False,
  tProfiles = []
}

buildProfiles :: [Profile] -> [(String, Profile)]
buildProfiles lst = [(pName p, p) | p <- lst]

