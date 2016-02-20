{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, DeriveDataTypeable, ExistentialQuantification #-}
module XMonad.Wacom.Kde
  (
    KDE (..),

    getProfile,
    setProfile,
    withProfile
  ) where

import Control.Exception
import Control.Monad (when)
import Data.Generics
import Data.Monoid (mconcat)
import Data.Maybe (fromMaybe)

import DBus
import DBus.Client
import DBus.TH

import XMonad
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

import qualified XMonad.Wacom as API

type StringList = [String]

-- KDE 5 interface
interface "org.kde.Wacom" "/Tablet" "org.kde.Wacom" Nothing
    [ "getTabletList" =:: Return ''StringList
    , "getProfile" =:: ''String :-> Return ''String `as` "kde5getProfile"
    , "setProfile" =:: ''String :-> ''String :-> Return ''Bool `as` "kde5setProfile"
    ]

-- KDE 4 interface
interface "org.kde.Wacom" "/Tablet" "org.kde.Wacom" Nothing
    [ "getProfile" =:: Return ''String `as` "kde4getProfile"
    , "setProfile" =:: ''String :-> Return ''Bool `as` "kde4setProfile"
    ]

data Wacom = 
     Wacom {
       wClient :: Client,
       wKdeVersion :: KdeVersion }
   | Unknown
  deriving (Typeable)

data KdeVersion = KDE4 | KDE5
  deriving (Eq, Show, Typeable)

data KDE = KDE

instance ExtensionClass Wacom where
  initialValue = Unknown

ensureConnection :: X Wacom
ensureConnection = do
  w <- XS.get
  case w of
    Unknown -> do
               dbus <- io connectSession
               v <- io $ (getTabletList dbus >> return KDE5)
                            `catch` \(e :: SomeException) -> return KDE4
               let wacom = Wacom dbus v
               XS.put wacom
               return wacom
    _ -> return w

getProfile :: X (Maybe String)
getProfile = do
    wacom <- ensureConnection
    io $ getProfile' (wClient wacom) (wKdeVersion wacom)
  where
    getProfile' dbus KDE4 = do
      p <- kde4getProfile dbus 
      case p of
        Nothing -> return Nothing
        Just [] -> return Nothing
        profile -> return profile
    getProfile' dbus KDE5 = do
      tablets <- getTabletList dbus
      case tablets of
        Just (tablet:_) -> kde5getProfile dbus tablet
        _ -> return Nothing

withProfile :: (String -> X ()) -> X ()
withProfile fn = do
  mbProfile <- getProfile
  whenJust mbProfile fn

setProfile :: String -> X ()
setProfile profile = do
    wacom <- ensureConnection
    io $ setProfile' (wClient wacom) (wKdeVersion wacom)
  where
    setProfile' dbus KDE4 = kde4setProfile dbus profile >> return ()
    setProfile' dbus KDE5 = do
      tablets <- getTabletList dbus
      case tablets of
        Just (tablet:_) -> kde5setProfile dbus tablet profile >> return ()
        _ -> return ()

instance API.ProfileApi KDE where
  getProfile _ = getProfile
  setProfile _ name = setProfile name

