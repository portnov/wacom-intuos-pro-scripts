import XMonad
import XMonad.Config.Kde (kde4Config)

import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Util.Replace

import XMonad.Wacom.Kde
import XMonad.Wacom

import Libnotify

notifySend :: String -> String -> String -> X ()
notifySend cat title text = do
  io $ display_ (timeout (Custom 2) <> category cat <> summary title <> body text)
  return ()

switchTabletProfile :: X ()
switchTabletProfile = 
    wacomProfiles KDE
      [(className =? "Krita", "Krita"),
       (className =? "gimp", "Gimp")]
      (\profile -> do
                   spawn "python /usr/local/bin/ring-mode.py --reset"
                   notifySend "Wacom" "Profile changed" ("Tablet profile changed to " ++ profile))

------------------------------------------------------------------------
-- General settings
--
baseConfig = kde4Config
baseManageHook = manageHook baseConfig
baseLogHook = logHook baseConfig

main =  do
  replace
  xmonad $ ewmh $ baseConfig {
        focusFollowsMouse  = False,
        modMask            = mod4Mask,
        workspaces         = map show [0..9],
        logHook            = switchTabletProfile
    }

