{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module System.Wacom.Matching where

import Control.Applicative
import Data.Aeson.Types (typeMismatch)
import Data.Yaml
import qualified Data.Text as T
import qualified Data.Vector as V

import qualified System.Wacom.Config as PC

data Condition = Condition {
    windowTitle :: Maybe String -- ^ Nothing - do not check
  , windowClass :: Maybe String -- ^ Nothing - do not check
  } deriving (Show)

instance FromJSON Condition where
  parseJSON (Object v) =
    Condition
      <$> v .:? "title"
      <*> v .:? "class"
  parseJSON invalid = typeMismatch "Condition" invalid

ifTitle :: String -> Condition 
ifTitle title = Condition (Just title) Nothing

ifClass :: String -> Condition
ifClass cls = Condition Nothing (Just cls)

-- | (Condition on window, corresponding profile name)
type Matchers = [(Condition, String)]

instance FromJSON Matchers where
  parseJSON (Array v) = mapM parseOne (V.toList v)
    where
      parseOne val@(Object obj) = do
        cond <- parseJSON val
        profile <- obj .: "profile"
        return (cond, profile)
      parseOne invalid = typeMismatch "Matching condition" invalid
  parseJSON invalid = typeMismatch "Matching conditions list" invalid

data Config = Config {
    mcProfilesConfig :: PC.Config,
    mcRingKey :: Maybe String,
    mcMatching :: Matchers
  } deriving (Show)

instance FromJSON Config where
  parseJSON val@(Object v) = do
    profilesConfig <- parseJSON val
    ringKey <- v .:? "ring-toggle-key"
    matching <- v .: "matching"
    return $ Config profilesConfig ringKey matching
  parseJSON invalid = typeMismatch "Configuration" invalid

data WindowInfo = WindowInfo {
    wiTitle :: String,
    wiClass :: String
  } deriving (Show)

match :: WindowInfo -> Condition -> Bool
match wi cond = 
  let titleOk = case windowTitle cond of
                  Nothing -> True
                  Just ct -> ct == wiTitle wi
      classOk = case windowClass cond of
                  Nothing -> True
                  Just cc -> cc == wiClass wi
  in titleOk && classOk

selectProfile :: Matchers -> WindowInfo -> String
selectProfile [] _ = "Default"
selectProfile ((cond, profile): xs) wi =
  if match wi cond
    then profile
    else selectProfile xs wi

readConfig :: FilePath -> IO (Either ParseException Config)
readConfig path = decodeFileEither path

