
module XMonad.Wacom where

import Control.Monad (when)
import Data.Maybe

import XMonad
import qualified XMonad.StackSet as W

class ProfileApi api where
  getProfile :: api -> X (Maybe String)
  setProfile :: api -> String -> X ()

wacomProfiles :: ProfileApi api => api -> [(Query Bool, String)] -> (String -> X ()) -> X ()
wacomProfiles api pairs onSwitch = do
    withWindowSet $ \ss -> do
      whenJust (W.peek ss) $ \window -> do
        op <- getProfile api
        case op of
          Nothing -> return ()
          Just oldProfile -> do
            newProfile <- selectProfile pairs window
            let newProfile' = fromMaybe "Default" newProfile
            when (oldProfile /= newProfile') $ do
              setProfile api newProfile'
              onSwitch newProfile'
  where
    selectProfile [] _ = return Nothing
    selectProfile ((qry, profile):ps) w = do
      matched <- runQuery qry w
      if matched
        then return (Just profile)
        else selectProfile ps w

