{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

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

import Control.Applicative
import Data.Char (isDigit)
import qualified Data.Map as M
import qualified Data.HashMap.Strict as H
import Data.Aeson.Types (typeMismatch)
import Data.Yaml
import qualified Data.Text as T

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

instance FromJSON Config where
  parseJSON (Object v) =
    Config
      <$> v .:? "stylus-device-key" .!= "Pen"
      <*> v .:? "pad-device-key" .!= "Pad"
      <*> v .:? "touch-device-key" .!= "Touch"
      <*> v .:? "enable-touch" .!= False
      <*> v .:? "mapping-areas" .!= []
      <*> (buildProfiles <$> (v .: "profiles"))
  parseJSON invalid = typeMismatch "Config" invalid

type Area = String

-- | Tablet settings profile
data Profile = Profile {
      pName :: String                    -- ^ Profile name
    , pRing :: [RingMode]                -- ^ Ring modes (for Intuos Pro models)
    , pButtons :: M.Map Int TabletAction -- ^ Tablet buttons bindings
  }
  deriving (Show)

instance FromJSON (M.Map Int TabletAction) where
  parseJSON (Object v) = do
      let lst = H.toList v
      lst' <- mapM go lst
      return $ M.fromList lst'
    where
      go (key,val) = do
        let keyStr = T.unpack key
        keyInt <- if all isDigit keyStr
                    then return $ read keyStr
                    else fail $ "Invalid button number " ++ keyStr
        val' <- parseJSON val
        return (keyInt, val')
          

instance FromJSON Profile where
  parseJSON (Object v) =
    Profile
      <$> v .: "name"
      <*> v .:? "ring" .!= []
      <*> v .:? "buttons" .!= M.empty
  parseJSON invalid = typeMismatch "Profile" invalid

-- | Intuos Pro ring mode
data RingMode = RingMode {
      rName :: String           -- ^ Mode name
    , ringUp :: TabletAction    -- ^ Action to be toggled at scrolling ring counterclockwise
    , ringDown :: TabletAction  -- ^ Action to be toggled at scrolling ring clockwise
  }
  deriving (Show)

instance FromJSON RingMode where
  parseJSON (Object v) =
    RingMode
      <$> v .: "name"
      <*> v .: "up"
      <*> v .: "down"
  parseJSON invalid = typeMismatch "RingMode" invalid

-- | Supported actions for binding
data TabletAction =
    Key String   -- ^ Keyboard shortcut, for example @shift@, @ctrl z@ etc
  | Click Int    -- ^ Mouse button click
  | DblClick Int -- ^ Mouse button double click

instance FromJSON TabletAction where
  parseJSON (String text) =
    case words (T.unpack text) of
      [] -> fail "empty tablet action"
      ("key":ws) -> return $ Key $ unwords ws
      ["button", s] ->
        if all isDigit s
          then return $ Click $ read s
          else fail $ "Invalid button number " ++ s
      ["dblclick", s] ->
        if all isDigit s
          then return $ DblClick $ read s
          else fail $ "Invalid button number " ++ s
      xs -> fail $ "Invalid tablet action specification: " ++ unwords xs
  parseJSON invalid = typeMismatch "TabletAction" invalid

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

readConfig :: FilePath -> IO (Either ParseException Config)
readConfig path = decodeFileEither path

