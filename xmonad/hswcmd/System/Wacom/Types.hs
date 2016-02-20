
module System.Wacom.Types where

data TabletDevice = TabletDevice {
    dStylus :: Maybe String,
    dPad :: Maybe String,
    dTouch :: Maybe String
  }
  deriving (Show)

noDevice :: TabletDevice
noDevice = TabletDevice Nothing Nothing Nothing

isEmpty :: TabletDevice -> Bool
isEmpty (TabletDevice Nothing Nothing Nothing) = True
isEmpty _ = False

isFull :: TabletDevice -> Bool
isFull (TabletDevice (Just _) (Just _) _) = True
isFull _ = False

type Result a = Either String a

