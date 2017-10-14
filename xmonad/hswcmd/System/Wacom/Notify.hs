
module System.Wacom.Notify where

import DBus.Notify as Notify

notify :: String -> String -> IO ()
notify title text = do
    dbus <- connectSession
    Notify.notify dbus $ blankNote {appName = cat, summary = title, body = Just $ Text text}
    return ()
  where
    cat = "Wacom Tablet"

