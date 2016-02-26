{-# LANGUAGE RecordWildCards #-}

module System.Wacom.X11
  (
    getWindowInfo,
    withDisplay,
    getNumlockMask,
    parseShortcut
  ) where

import Control.Monad
import Control.Exception as E
import Data.Bits

import Graphics.X11
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xlib.Atom

import System.Wacom.Matching

getTitle :: Display -> Window -> IO String
getTitle d w = do
    let getProp =
            (internAtom d "_NET_WM_NAME" False >>= getTextProperty d w)
                `E.catch` \(SomeException e) -> do
                                                putStrLn (show e)
                                                getTextProperty d w wM_NAME
        extract prop = do l <- wcTextPropertyToTextList d prop
                          return $ if null l then "<?>" else head l
    bracket getProp (xFree . tp_value) extract `E.catch` \(SomeException e) -> return (show e)

getWindow :: Display -> Atom -> Window -> IO (Maybe Window)
getWindow dpy atom root = do
  r <- getWindowProperty32 dpy atom root
  case r of
    Just [w] -> return $ Just $ fromIntegral w
    _ -> return Nothing

-- | Obtain basic information about currently active window
getWindowInfo :: Display
              -> Atom                 -- ^ _NET_ACTIVE_WINDOW atom
              -> Window               -- ^ X root window
              -> IO (Maybe WindowInfo)
getWindowInfo dpy atom root = do
  r <- getWindow dpy atom root
  case r of
    Nothing -> return Nothing
    Just 0 -> return Nothing
    Just window -> do
      title <- getTitle dpy window
      cls <- resName `fmap` getClassHint dpy window
      return $ Just $ WindowInfo title cls

withDisplay :: String -> (Display -> IO a) -> IO a
withDisplay str fn = do
  dpy <- openDisplay str
  res <- fn dpy
  closeDisplay dpy
  return res

-- | Detect KeyMask corresponding to NumLock modifier
getNumlockMask :: Display -> IO KeyMask
getNumlockMask dpy = do
    ms <- getModifierMapping dpy
    xs <- sequence [ do
                        ks <- keycodeToKeysym dpy kc 0
                        if ks == xK_Num_Lock
                            then return (setBit 0 (fromIntegral m))
                            else return (0 :: KeyMask)
                        | (m, kcs) <- ms, kc <- kcs, kc /= 0]
    return $ foldr (.|.) 0 xs 

-- | Parse X11 KeyMask + KeySym from xsetwacom-style specification.
--
-- Supported are strings like @F11@, @ctrl alt z@.
parseShortcut :: String -> Maybe (Modifier, KeySym)
parseShortcut str = do
    let ws = words str
    case ws of
      [] -> Nothing
      [w] -> do
             k <- parseKey w
             return (noModMask, k)
      _ -> do
           mods <- parseMods (init ws)
           k <- parseKey (last ws)
           return (mods, k)
  where
    parseKey :: String -> Maybe KeySym
    parseKey key = 
      case stringToKeysym key of
               0 -> Nothing
               k -> Just k

    parseMods :: [String] -> Maybe Modifier
    parseMods mods = foldr (.|.) 0 `fmap` mapM parseMod mods

    parseMod :: String -> Maybe Modifier
    parseMod "ctrl" = Just controlMask 
    parseMod "shift" = Just shiftMask
    parseMod "alt" = Just mod1Mask
    parseMod _ = Nothing

