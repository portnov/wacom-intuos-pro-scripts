{-# LANGUAGE DeriveDataTypeable #-}

module XMonad.Wacom.Daemon
  (
    Internal (..),
    initWacom,
    getProfile,
    setProfile
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

data Internal = Internal

initWacom :: Config -> X ()
initWacom config = do
  wh <- io $ Profiles.newWacomHandle config
  XS.put $ Inited config wh
  io $ Daemon.initUdevMonitor wh
  ensureDaemonRunning
  return ()

ensureDaemonRunning :: X Wacom
ensureDaemonRunning = do
  w <- XS.get
  case w of
    NotInited -> fail "ensureDaemonRunning should be called after initWacom!"
    Inited config wh -> do
        io $ do
            forkIO $ Daemon.udevMonitor wh
            Profiles.setProfile wh "Default"
            Profiles.setRingMode wh 0
            Profiles.setMapArea wh 0
            return ()
        let wacom = Wacom wh
        XS.put wacom
        return wacom
    _ -> return w

getProfile :: X (Maybe String)
getProfile = do
    Wacom wh <- ensureDaemonRunning
    r <- io $ Profiles.getProfileName wh
    case r of
      Left _ -> return Nothing
      Right name -> return (Just name)

setProfile :: String -> X ()
setProfile name = do
    Wacom wh <- ensureDaemonRunning
    io $ Profiles.setProfile wh name
    return ()

instance API.ProfileApi Internal where
  getProfile _ = getProfile
  setProfile _ name = setProfile name

