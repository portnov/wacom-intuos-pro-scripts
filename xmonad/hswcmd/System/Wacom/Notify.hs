
module System.Wacom.Notify where

import Libnotify

notify :: String -> String -> IO ()
notify title text = do
    display_ (category cat <> summary title <> body text)
  where
    cat = "Wacom Tablet"

