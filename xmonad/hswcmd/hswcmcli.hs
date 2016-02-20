{-# LANGUAGE OverloadedStrings #-}

import Control.Monad

import Data.List
import Data.Char
import qualified Data.ByteString as B
import qualified Data.Map as M

import Control.Concurrent

import System.Process
import System.IO

import System.Wacom.Daemon
import System.Wacom.CLI
import System.Wacom.Profiles

import Config

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  wh <- newWacomHandle config
  forkIO $ udevMonitor wh
  setProfile wh "Default"
  setRingMode wh 0
  print =<< setMapArea wh 0
  client wh

