{-# LANGUAGE RecordWildCards #-}

import Control.Monad
import qualified Data.Map as M
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

runModeAction :: WacomHandle -> Config -> ModeAction -> IO ()
runModeAction wh cfg ToggleRingMode = do
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
runModeAction wh _ (SetMappingArea idx) = do
    setMapArea wh idx
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
  setMapArea wh 0


  withDisplay "" $ \dpy -> do
    let rootw = defaultRootWindow dpy
    numlock <- getNumlockMask dpy
    grabHotkeys dpy rootw numlock $ getHotkeys (mcKeyMap cfg)
    keymap <- compileKeyMap dpy $ mcKeyMap cfg
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
            -- Key pressed
            when (ev_event_type == keyPress) $ do
                keysym <- keycodeToKeysym dpy ev_keycode 0
                let mask = cleanMask numlock ev_state
                case M.lookup (mask,keysym) keymap of
                  Nothing -> putStrLn $ "Unknown key " ++ show (mask,keysym)
                  Just action -> runModeAction wh cfg action
                return ()
          _ -> putStrLn $ "Unexpected event: " ++ show ev

