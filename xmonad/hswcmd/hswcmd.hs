{-# LANGUAGE RecordWildCards #-}

import Control.Monad
import Control.Exception as E
import Control.Concurrent
import Foreign
import Foreign.Ptr
import Text.Printf

import Graphics.X11
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xlib.Atom

import System.Wacom.Daemon
import System.Wacom.CLI
import System.Wacom.Profiles
import System.Wacom.Matching

import Config

matchConfig :: Matchers
matchConfig =
  [(ifClass "krita", "Krita"),
   (ifClass "Gimp", "Gimp"),
   (ifClass "Blender", "Blender")
  ]

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

getWindowInfo :: Display -> Atom -> Window -> IO (Maybe WindowInfo)
getWindowInfo dpy atom root = do
  r <- getWindow dpy atom root
  case r of
    Nothing -> return Nothing
    Just 0 -> return Nothing
    Just window -> do
      title <- getTitle dpy window
      cls <- resName `fmap` getClassHint dpy window
      return $ Just $ WindowInfo title cls

main :: IO ()
main = do
  wh <- newWacomHandle config
  forkIO $ udevMonitor wh
  setProfile wh "Default"
  setRingMode wh 0
  print =<< setMapArea wh 0

  dpy <- openDisplay ""
  let rootw = defaultRootWindow dpy
  selectInput dpy rootw propertyChangeMask
  forever $ do
    allocaXEvent $ \xptr -> do
      nextEvent dpy xptr
      ev <- getEvent xptr
      case ev of
        PropertyEvent {..} -> do
          mbName <- getAtomName dpy ev_atom
          case mbName of
            Just "_NET_ACTIVE_WINDOW" -> do
              mbWindow <- getWindowInfo dpy ev_atom ev_window
              case mbWindow of
                Just wi -> do
                  print wi
                  rOld <- getProfileName wh
                  let newProfile = selectProfile matchConfig wi
                  putStrLn $ "Selecting profile: " ++ newProfile
                  case rOld of
                    Right old -> do
                      putStrLn $ "Old profile: " ++ old
                      when (newProfile /= old) $ do
                             setProfile wh newProfile
                             return ()

                _ -> return ()
            _ -> return ()
        
  closeDisplay dpy
