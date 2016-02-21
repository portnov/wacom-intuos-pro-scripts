
module System.Wacom.Ring
  (
    ControlFile,
    getControlFile,
    readMode,
    setMode
  )
  where

import System.FilePath.Glob
import System.Directory

type ControlFile = FilePath

control_path_mask = "/sys/bus/usb/devices/*/*/wacom_led/status_led0_select"

-- | Check whether file is writeable
isWritable :: FilePath -> IO Bool
isWritable path = writable `fmap` getPermissions path

getControlFile :: IO (Maybe ControlFile)
getControlFile = do
  files <- glob control_path_mask
  if null files
    then return Nothing
    else return $ Just $ head files

readMode :: ControlFile -> IO Int
readMode path = do
  str <- readFile path
  return $ read str

setMode :: ControlFile -> Int -> IO ()
setMode path idx = do
  writeFile path (show idx)
