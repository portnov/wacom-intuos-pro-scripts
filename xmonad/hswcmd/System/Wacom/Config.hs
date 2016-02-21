
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

-- | Tablet configuration
data Config = Config {
      tStylusKey :: String              -- ^ Keyword to be used to detect Wacom Stylus device name. Default is @Pen@.
    , tPadKey :: String                 -- ^ Keyword to be used to detect Wacom Pad device name. Default is @Pad@.
    , tTouchKey :: String               -- ^ Keyword to be used to detect Wacom Touch device name. Default is @Touch@.
    , tTouch :: Bool                    -- ^ Whether to enable touchpad functionality (if supported by tablet).
    , tMapAreas :: [Area]               -- ^ List of predefined tablet mapping areas (for example, different monitors).
                                        --   Specified in X11 Geometry format, for example @1920x1200+0+0@.
    , tProfiles :: [(String, Profile)]  -- ^ Map of named settings profiles.
  }
  deriving (Show)

type Area = String

-- | Tablet settings profile
data Profile = Profile {
      pName :: String                    -- ^ Profile name
    , pRing :: [RingMode]                -- ^ Ring modes (for Intuos Pro models)
    , pButtons :: M.Map Int TabletAction -- ^ Tablet buttons bindings
  }
  deriving (Show)

-- | Intuos Pro ring mode
data RingMode = RingMode {
      rName :: String           -- ^ Mode name
    , ringUp :: TabletAction    -- ^ Action to be toggled at scrolling ring counterclockwise
    , ringDown :: TabletAction  -- ^ Action to be toggled at scrolling ring clockwise
  }
  deriving (Show)

-- | Supported actions for binding
data TabletAction =
    Key String   -- ^ Keyboard shortcut, for example @shift@, @ctrl z@ etc
  | Click Int    -- ^ Mouse button click
  | DblClick Int -- ^ Mouse button double click

instance Show TabletAction where
  show (Key s) = "key " ++ s
  show (Click b) = "button " ++ show b
  show (DblClick b) = "dblclick " ++ show b

-- | Default config
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

