
module System.Wacom.Profiles
  (
    WacomHandle,

    newWacomHandle,
    setProfile,
    getProfileName,
    setRingMode,
    toggleRingMode,
    getRingMode,
    syncRingMode,
    initRingControlFile,
    setMapArea,
    getMapArea,

    emptyState,

    runProfile,
    xsetwacom
  )
  where

import Control.Monad
import qualified Data.Map as M
import Data.Maybe
import Text.Printf
import System.Process

import Control.Concurrent

import System.Wacom.Types
import System.Wacom.Internal
import System.Wacom.Config
import System.Wacom.Internal
import qualified System.Wacom.Ring as Ring

emptyState :: Config -> MonState
emptyState cfg = MonState noDevice cfg Nothing Nothing Nothing Nothing

newWacomHandle :: Config -> IO WacomHandle
newWacomHandle cfg = do
  tvar <- newMVar $ emptyState cfg
  let wh = WacomHandle tvar
  return wh

renderTouch :: TabletDevice -> Config -> [String]
renderTouch td cfg =
  case dTouch td of
    Just touch -> [printf "\"%s touch\" Touch %s" touch on]
    Nothing -> []
  where
    on | tTouch cfg = "on"
       | otherwise  = "off"

renderRingMode :: TabletDevice -> RingMode -> [String]
renderRingMode td r = 
  case dPad td of
    Just pad ->
        [printf "\"%s pad\" AbsWheelDown \"%s\"" pad (show $ ringDown r),
         printf "\"%s pad\" AbsWheelUp \"%s\"" pad (show $ ringUp r)]
    Nothing -> []

renderButtons :: TabletDevice -> M.Map Int TabletAction -> [String]
renderButtons td m =
  case dPad td of
    Nothing -> []
    Just pad ->
      [printf "\"%s pad\" Button %d \"%s\"" pad idx (show action)
          | (idx,action) <- M.assocs m]

renderMapArea :: TabletDevice -> String -> [String]
renderMapArea td area =
  case dStylus td of
    Nothing -> []
    Just stylus ->
      [printf "\"%s stylus\" MapToOutput %s" stylus area]

runProfile :: TabletDevice -> Config -> Maybe Profile -> Maybe RingMode -> IO ()
runProfile td cfg mbProfile mbMode = do
  let rmode = case mbMode of
                Nothing -> []
                Just ringMode -> renderRingMode td ringMode
      prof = case mbProfile of
                Nothing -> []
                Just profile -> renderButtons td (pButtons profile)
  xsetwacom $ 
    renderTouch td cfg ++
    rmode ++
    prof

xsetwacom :: [String] -> IO ()
xsetwacom cmds = do
  forM_ cmds $ \cmd -> do
    let command = "xsetwacom set " ++ cmd
    putStrLn command
    spawnCommand command

setProfile :: WacomHandle -> String -> IO (Result String)
setProfile (WacomHandle tvar) profileName = do
  st <- readMVar tvar
  let cfg = msConfig st
      mbMode = msRingMode st
      td = msDevice st
  case lookup profileName (tProfiles cfg) of
    Nothing -> return $ Left $ "Unknown profile " ++ profileName
    Just profile -> do
        runProfile td cfg (Just profile) (snd `fmap` mbMode)
        modifyMVar_ tvar $ \st -> return $ st {msProfile = Just profile}
        return $ Right profileName

setRingMode :: WacomHandle -> Int -> IO (Result String)
setRingMode (WacomHandle tvar) idx = do
  st <- readMVar tvar
  let cfg = msConfig st
      td = msDevice st
  case msProfile st of
    Nothing -> return $ Left "No current profile, can't set ring mode"
    Just profile ->
      if (idx >= 0) && (idx < length (pRing profile))
        then do
             let rmode = pRing profile !! idx
             runProfile td cfg Nothing (Just rmode)
             case msRingControl st of
               Nothing -> putStrLn "No ring control file"
               Just file -> Ring.setMode file idx
             modifyMVar_ tvar $ \st -> return $ st {msRingMode = Just (idx, rmode)}
             return $ Right $ show idx
        else return $ Left $ "Invalid ring mode index"

toggleRingMode :: WacomHandle -> IO (Result String)
toggleRingMode wh@(WacomHandle tvar) = do
  st <- readMVar tvar
  let mbMode = msRingMode st
  case msProfile st of
    Nothing -> return $ Left "No current profile, can't set ring mode"
    Just profile ->
      case mbMode of
        Nothing -> return $ Left $ "No current ring mode, cannot toggle"
        Just (idx, rmode) -> do
          let n = length (pRing profile)
              idx' = (idx + 1) `mod` n
          setRingMode wh idx'

getRingMode :: WacomHandle -> IO (Result String)
getRingMode (WacomHandle tvar) = do
  st <- readMVar tvar
  case msRingMode st of
    Nothing -> return $ Left "No ring mode"
    Just rmode -> return $ Right $ show rmode

syncRingMode :: WacomHandle -> IO (Result String)
syncRingMode wh@(WacomHandle tvar) = do
  st <- readMVar tvar
  case msRingControl st of
    Nothing -> return $ Left "No ring control file"
    Just file -> do
      idx <- Ring.readMode file
      setRingMode wh idx

getProfileName :: WacomHandle -> IO (Result String)
getProfileName (WacomHandle tvar) = do
  st <- readMVar tvar
  case msProfile st of
    Nothing -> return $ Left "No current profile"
    Just profile -> return $ Right $ pName profile

initRingControlFile :: WacomHandle -> IO (Result String)
initRingControlFile (WacomHandle tvar) = do
  r <- Ring.getControlFile
  modifyMVar_ tvar $ \st -> return $ st {msRingControl = r}
  case r of
    Nothing -> return $ Left "No ring control file"
    Just file -> return $ Right file

setMapArea :: WacomHandle -> Int -> IO (Result String)
setMapArea (WacomHandle tvar) idx = do
  st <- readMVar tvar
  let td = msDevice st
      cfg = msConfig st
  case tMapAreas cfg of
    [] -> return $ Left $ "No map areas defined"
    areas ->
      if (idx >= 0) && (idx < length areas)
        then do
             let area = areas !! idx
             xsetwacom $ renderMapArea td area
             modifyMVar_ tvar $ \st -> return $ st {msArea = Just area}
             return $ Right area
        else return $ Left "Invalid map area index"

getMapArea :: WacomHandle -> IO (Result String)
getMapArea (WacomHandle tvar) = do
  st <- readMVar tvar
  case msArea st of
    Nothing -> return $ Left "No current map area"
    Just area -> return $ Right area

