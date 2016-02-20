
module System.Wacom.Internal
  (
    MonState (..),
    WacomHandle (..)
  )
  where

import Control.Concurrent

import System.Wacom.Types
import System.Wacom.Config
import qualified System.Wacom.Ring as Ring

data MonState = MonState {
    msDevice :: TabletDevice,
    msConfig :: Config,
    msArea :: Maybe Area,
    msProfile :: Maybe Profile,
    msRingMode :: Maybe (Int, RingMode),
    msRingControl :: Maybe Ring.ControlFile
  }
  deriving (Show)

newtype WacomHandle = WacomHandle (MVar MonState)

