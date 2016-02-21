
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

-- | UDev monitor internal state
data MonState = MonState {
      msDevice :: TabletDevice                 -- ^ Currently attached tablet device
    , msConfig :: Config                       -- ^ Configuration
    , msArea :: Maybe Area                     -- ^ Currently selected mapping area
    , msProfile :: Maybe Profile               -- ^ Currently selected profile
    , msRingMode :: Maybe (Int, RingMode)      -- ^ Currently selected ring mode: index and definition
    , msRingControl :: Maybe Ring.ControlFile  -- ^ Ring control file
  }
  deriving (Show)

-- | Abstract data type - handle to communicate with daemon.
newtype WacomHandle = WacomHandle (MVar MonState)

