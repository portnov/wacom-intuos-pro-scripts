{-# LANGUAGE OverloadedStrings #-}

module System.Wacom.Daemon
  where

import Control.Monad

import Data.List

import Control.Concurrent

import System.UDev hiding (isEmpty)
import System.Posix.IO.Select
import System.Posix.IO.Select.Types hiding (Result)

import System.Wacom.Types
import System.Wacom.Internal
import System.Wacom.Util
import System.Wacom.Profiles
import System.Wacom.Config

-- | Detect tablet devices by using udev
detectAtStartup :: Config -> UDev -> IO (Maybe TabletDevice)
detectAtStartup cfg udev = do
    list <- initEnumeration
    deviceNames <- enumerateDevices list
    let wacomDevices = filter isWacom $ map fromBS deviceNames
    if null wacomDevices
      then return Nothing
      else do
           let names = map (trim . trimQuotes) wacomDevices
               stylus = pick (tStylusKey cfg) names
               pad    = pick (tPadKey cfg) names
               touch  = pick (tTouchKey cfg) names
               dev = TabletDevice stylus pad touch
           return $ Just dev
  where
    initEnumeration = do 
      enum <- newEnumerate udev
      addMatchSubsystem enum "input"
      scanDevices enum
      list <- getListEntry enum
      return list

    enumerateDevices list = iter list

    iter (Just list) = do
      path <- getName list
      dev <- newFromSysPath udev path
      mbName <- getPropertyValue dev "NAME"
      x <- getNext list
      rest <- iter x
      case mbName of
        Nothing -> return rest
        Just new -> do
            -- C8.putStrLn $ "Device: " `B.append` new
            return (new : rest)
    iter Nothing = do
      putStrLn "End."
      return []

    pick _ [] = Nothing
    pick suffix (name : names)
      | suffix `isSuffixOf` name = Just name
      | otherwise = pick suffix names

-- | To be called before starting udevMonitor.
initUdevMonitor :: WacomHandle -> IO ()
initUdevMonitor wh@(WacomHandle tvar) = do
    putStrLn "Udev monitor initing..."
    return ()

-- | UDev device monitor daemon.
-- To be called in separate thread (by forkIO).
-- This daemon implements the following functionality:
--
--  * Detects attached tablet at startup (via udev).
--
--  * Detects tablet plugging while running (via udev).
--
--  * Tracks selected settings profile and ring mode.
--
--  * Applies selected settings by calling xsetwacom automatically
--    after tablet is attached.
--
udevMonitor :: WacomHandle -> IO ()
udevMonitor wh@(WacomHandle tvar) = withUDev $ \udev -> do
    putStrLn "Udev monitor starting..."

    st <- readMVar tvar
    let cfg = msConfig st
    mbDev <- detectAtStartup cfg udev
    case mbDev of
      Just dev -> do
                  modifyMVar_ tvar $ \st ->
                             return st {msDevice = dev}
      _ -> return ()

    st <- readMVar tvar
    onPlug (msConfig st)
    monitor <- newFromNetlink udev UDevId
    filterAddMatchSubsystemDevtype monitor "input" Nothing
    enableReceiving monitor
    fd <- getFd monitor
    forever $ do
      res <- select' [fd] [] [] Never
      case res of
        Just ([_], [], []) -> do
          dev <- receiveDevice monitor
          st <- readMVar tvar
          handleDevice (msConfig st) dev
        Nothing -> return ()
    return ()
  where
    handleDevice :: Config -> Device -> IO ()
    handleDevice cfg dev = do
      mbName <- getPropertyValue dev "NAME"
      let mbAction = getAction dev
      case (mbName, mbAction) of
        (Just name, Just action) -> handle cfg action (trimQuotes $ trim $ fromBS name)
        _ -> return ()

    handle :: Config -> Action -> String -> IO ()
    handle cfg Add name
      | isWacom name && (tStylusKey cfg) `isInfixOf` name = setStylus $ Just name
      | isWacom name && (tPadKey cfg) `isInfixOf` name = setPad $ Just name
      | isWacom name && (tTouchKey cfg) `isInfixOf` name = setTouch $ Just name
      | otherwise = putStrLn $ "Add " ++ name
    handle cfg Remove name
      | isWacom name && (tStylusKey cfg) `isInfixOf` name = setStylus Nothing
      | isWacom name && (tPadKey cfg) `isInfixOf` name = setPad Nothing
      | isWacom name && (tTouchKey cfg) `isInfixOf` name = setTouch Nothing
      | otherwise = putStrLn $ "Remove " ++ name
    handle _ action name =
      putStrLn $ show action ++ " " ++ name

    setStylus :: Maybe String -> IO ()
    setStylus x = do
      putStrLn $ "Detected stylus: " ++ show x
      modifyMVar_ tvar $ \st -> return $ st {msDevice = (msDevice st) {dStylus = x}}
      st <- readMVar tvar
      when (isFull $ msDevice st) $ onPlug (msConfig st)
      when (isEmpty $ msDevice st) onUnplug

    setPad :: Maybe String -> IO ()
    setPad x = do
      putStrLn $ "Detected pad: " ++ show x
      modifyMVar_ tvar $ \st -> return $ st {msDevice = (msDevice st) {dPad = x}}
      st <- readMVar tvar
      when (isFull $ msDevice st) $ onPlug (msConfig st)
      when (isEmpty $ msDevice st) onUnplug

    setTouch :: Maybe String -> IO ()
    setTouch x = do
      putStrLn $ "Detected touch: " ++ show x
      modifyMVar_ tvar $ \st -> return $ st {msDevice = (msDevice st) {dTouch = x}}
      st <- readMVar tvar
      when (isFull $ msDevice st) $ onPlug (msConfig st)
      when (isEmpty $ msDevice st) onUnplug

    onPlug :: Config -> IO ()
    onPlug cfg = do
      st <- readMVar tvar
      let dev = msDevice st
      putStrLn $ "Plugged " ++ show dev
      let profile = msProfile st
          rmode = msRingMode st
      initRingControlFile wh
      runProfile dev cfg profile (snd `fmap` rmode)
      return ()

    onUnplug :: IO ()
    onUnplug = do
      d <- readMVar tvar
      putStrLn $ "Unplugged " ++ show (msDevice d)

