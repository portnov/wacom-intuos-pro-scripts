{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent

import System.IO

import System.Wacom.Daemon
import System.Wacom.CLI
import System.Wacom.Profiles

import Config

-- Simple command-line utility to select tablet profiles

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  wh <- newWacomHandle config
  forkIO $ udevMonitor wh
  setProfile wh "Default"
  setRingMode wh 0
  print =<< setMapArea wh 0
  client wh

