{-# LANGUAGE DeriveDataTypeable #-}
-- | This module contains implementation of switching Wacom tablet settings profiles
-- via xsetwacom command-line utility.
-- This also contains detection of Wacom tablet devices via udev library.
module XMonad.Wacom.Daemon
  (
    Internal (..),
    initWacom,
    getProfile,
    setProfile,
    setTabletMapArea
  )
  where

import Control.Concurrent
import Data.Generics

import XMonad
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

import System.Wacom.Config
import qualified System.Wacom.Daemon as Daemon
import qualified System.Wacom.Profiles as Profiles

import qualified XMonad.Wacom as API

data Wacom =
    Wacom Profiles.WacomHandle
  | NotInited
  | Inited Config Profiles.WacomHandle
  deriving (Typeable)

instance ExtensionClass Wacom where
  initialValue = NotInited

-- | Dummy data type for API implementation
data Internal = Internal

-- | Init udev monitor daemon.
-- This should be run from XMonad startupHook.
initWacom :: Config -> X ()
initWacom config = do
  wh <- io $ Profiles.newWacomHandle config
  XS.put $ Inited config wh
  io $ Daemon.initUdevMonitor wh
  ensureDaemonRunning
  return ()

-- | Return handle to running daemon
-- or run new daemon.
ensureDaemonRunning :: X Wacom
ensureDaemonRunning = do
  w <- XS.get
  case w of
    NotInited -> return NotInited 
    Inited config wh -> do
        io $ do
            forkIO $ Daemon.udevMonitor wh
            Profiles.setProfile wh "Default"
            Profiles.setRingMode wh 0
            r <- Profiles.setMapArea wh 0
            case r of
              Left err -> putStrLn err
              _ -> return ()
            return ()
        let wacom = Wacom wh
        XS.put wacom
        return wacom
    _ -> return w

-- | Return name of currently selected profile.
-- Returns Nothing if there is no tablet attached
-- or no profile selected.
getProfile :: X (Maybe String)
getProfile = do
    w <- ensureDaemonRunning
    case w of
      Wacom wh -> do
        r <- io $ Profiles.getProfileName wh
        case r of
          Left err -> do
            io $ putStrLn err
            return Nothing
          Right name -> return (Just name)
      _ -> do
          io $ putStrLn "getProfile should be called after initWacom!"
          return Nothing

-- | Set profile by name
setProfile :: String -> X ()
setProfile name = do
    w <- ensureDaemonRunning
    case w of
      Wacom wh -> do
        r <- io $ Profiles.setProfile wh name
        case r of
          Left err -> io $ putStrLn err
          _ -> return ()
        return ()
      _ -> do
        io $ putStrLn "setProfile should be called after initWacom!"
        return ()

-- | Set tablet mapping area by index (numbering from zero).
-- Areas themselve are defined in tMapAreas field of Config.
setTabletMapArea :: Int -> X ()
setTabletMapArea idx = do
    w <- ensureDaemonRunning
    case w of
      Wacom wh -> do
        r <- io $ Profiles.setMapArea wh idx
        case r of
          Left err -> io $ putStrLn err
          Right _ -> return ()
      _ -> io $ putStrLn "setTabletMapArea should be called after initWacom!"

instance API.ProfileApi Internal where
  getProfile _ = getProfile
  setProfile _ name = setProfile name

