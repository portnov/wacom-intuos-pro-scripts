-- | Generic API module
module XMonad.Wacom where

import Control.Monad (when)
import Data.Maybe

import XMonad
import qualified XMonad.StackSet as W

-- | Class for different implementations of
-- profile switching API
class ProfileApi api where
  getProfile :: api -> X (Maybe String)
  setProfile :: api -> String -> X ()

-- | XMonad LogHook which automatically selects Wacom tablet settings profile
-- depending on current window.
wacomProfiles :: ProfileApi api => api
             -> [(Query Bool, String)]     -- ^ [(Condition on window; profile name)]
             -> (String -> X ())           -- ^ Callback to be called after switching to new profile
             -> X ()
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

