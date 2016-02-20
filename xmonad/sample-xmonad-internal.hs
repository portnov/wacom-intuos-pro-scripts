import qualified Data.Map as M

import XMonad
import XMonad.Config.Kde (kde4Config)

import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Util.Replace

import System.Wacom.Config
import XMonad.Wacom.Daemon
import XMonad.Wacom

import Libnotify

notifySend :: String -> String -> String -> X ()
notifySend cat title text = do
  io $ display_ (timeout (Custom 2) <> category cat <> summary title <> body text)
  return ()

switchTabletProfile :: X ()
switchTabletProfile = 
    wacomProfiles Internal
      [(className =? "Krita", "Krita"),
       (className =? "gimp", "Gimp")]
      (\profile -> do
                   notifySend "Wacom" "Profile changed" ("Tablet profile changed to " ++ profile))

------------------------------------------------------------------------
-- General settings
--
baseConfig = kde4Config
baseManageHook = manageHook baseConfig
baseLogHook = logHook baseConfig

wconfig :: Config
wconfig =
  dfltConfig {
    tMapAreas = ["1920x1200+0+0", "1920x1080+1920+0"],
    tProfiles = buildProfiles
                 [Profile "Default" [ringScrolls, ringBrackets] buttons]
  }

ringScrolls :: RingMode
ringScrolls = RingMode "Scroll" (Click 5) (Click 4)

ringBrackets :: RingMode
ringBrackets = RingMode "Brackets" (Key "[") (Key "]")

buttons :: M.Map Int TabletAction
buttons = M.fromList [
           (2, Key "ctrl"),
           (3, Key "shift"),
           (13, Key "ctrl z"),
           (12, Key "Delete")
          ]

main =  do
  replace
  xmonad $ ewmh $ baseConfig {
        focusFollowsMouse  = False,
        modMask            = mod4Mask,
        workspaces         = map show [0..9],
        startupHook        = initWacom wconfig,
        logHook            = switchTabletProfile
    }

