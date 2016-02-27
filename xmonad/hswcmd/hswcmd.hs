{-# LANGUAGE RecordWildCards #-}

import Control.Monad
import Control.Exception as E
import Control.Concurrent
import Foreign
import Foreign.Ptr
import Data.Bits ((.|.))
import Text.Printf

import Graphics.X11
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xlib.Atom

import System.FilePath
import System.Environment

import System.Wacom.Daemon
import System.Wacom.CLI
import System.Wacom.Profiles
import System.Wacom.X11
import System.Wacom.Matching
import System.Wacom.Notify

onCurrentWindowChange :: WacomHandle -> Config -> WindowInfo -> IO ()
onCurrentWindowChange wh cfg wi = do
    print wi
    rOld <- getProfileName wh
    let newProfile = selectProfile (mcMatching cfg) wi
    case rOld of
      Left err -> putStrLn $ "Cant get old profile: " ++ err
      Right old -> do
        putStrLn $ "Old profile: " ++ old
        when (newProfile /= old) $ do
               r <- setProfile wh newProfile
               case r of
                 Right p -> do
                     putStrLn $ "Selecting profile: " ++ p
                     when (mcNotify cfg) $
                         notify "Tablet profile" $ "New tablet profile selected: " ++ p
                 Left err -> putStrLn $ "Error: " ++ err
               return ()

getConfigFile :: IO FilePath
getConfigFile = do
  home <- getEnv "HOME"
  return $ home </> ".config" </> "hswcmd.yaml"

main :: IO ()
main = do
  ecfg <- readConfig =<< getConfigFile
  cfg <- case ecfg of
           Left err -> fail $ show err
           Right cfg -> return cfg

  wh <- newWacomHandle (mcProfilesConfig cfg)
  forkIO $ udevMonitor wh
  setProfile wh "Default"
  setRingMode wh 0
  print =<< setMapArea wh 0

  withDisplay "" $ \dpy -> do
    let rootw = defaultRootWindow dpy
    case mcRingKey cfg of
      Nothing -> return ()
      Just keystr -> do
        case parseShortcut keystr of
          Nothing -> fail $ "Unsupported shortcut for ring mode toggle: " ++ keystr
          Just (mod, keysym) -> do
            hotkey <- keysymToKeycode dpy keysym
            numlock <- getNumlockMask dpy
            -- Grab both mask+key and numlock+mask+key.
            grabKey dpy hotkey mod rootw True grabModeAsync grabModeAsync
            grabKey dpy hotkey (mod .|. numlock) rootw True grabModeAsync grabModeAsync
    -- We need only PropertyEvent (toggled when active window is changed)
    -- and KeyEvent (when keyboard key is pressed)
    selectInput dpy rootw (propertyChangeMask .|. keyPressMask)
    forever $ do
      allocaXEvent $ \xptr -> do
        nextEvent dpy xptr
        ev <- getEvent xptr
        case ev of
          PropertyEvent {..} -> do
            mbName <- getAtomName dpy ev_atom
            case mbName of
              -- We are interested only in such events - active window changed
              Just "_NET_ACTIVE_WINDOW" -> do
                mbWindow <- getWindowInfo dpy ev_atom ev_window
                case mbWindow of
                  Just wi -> onCurrentWindowChange wh cfg wi
                  Nothing -> return ()
              _ -> return ()
          KeyEvent {..} -> do
            when (ev_event_type == keyPress) $ do
                r <- toggleRingMode wh
                case r of
                  Left err -> putStrLn $ "Error: " ++ err
                  Right res -> do
                      putStrLn $ "Ring mode: " ++ res
                      when (mcNotify cfg) $ do
                          rm <- getRingModeName wh
                          case rm of
                            Left err -> putStrLn $ "Error: " ++ err
                            Right name -> notify "Ring Mode" $ "New ring mode selected: " ++ name
                return ()
          _ -> putStrLn $ "Unexpected event: " ++ show ev

