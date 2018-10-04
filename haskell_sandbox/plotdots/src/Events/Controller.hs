{-# Language MultiWayIf #-}

module Events.Controller where

import           Control.Concurrent            (forkIO,threadDelay,ThreadId,killThread)
import           Control.Concurrent.MVar
import           Control.Monad                 (forM_)
import           Control.Monad.State.Lazy      (StateT, runStateT, get, put, lift)
-- my modules
import           Events                        (Events(..))
import           Keyboard.Controller           (getEventFromKeyboard)
import qualified Timing                  as TC
import           View


waitForEvents :: IO (MVar Events, ThreadId)
waitForEvents = do
    threadDelay 10
    waitForInputKeyboad

waitForInputKeyboad :: IO (MVar Events, ThreadId)
waitForInputKeyboad = do
    kStatus <- newEmptyMVar :: IO (MVar Events)
    id <- forkIO $ forM_ (repeat ()) $ \_ -> do
        setKeyboardEvent kStatus
    return (kStatus, id)

setKeyboardEvent :: MVar Events -> IO ()
setKeyboardEvent mv = getEventFromKeyboard >>= putMVar mv


data AppState = Living
              | Dead

untilQuit :: (Events -> IO ()) -> MVar Events -> IO AppState
untilQuit act eS = do
    mE <- recieveEvents eS
    if isQuited mE then
        return Dead
    else do
        activateEvent act mE
        return Living
    where
        recieveEvents :: MVar Events -> IO (Maybe Events)
        recieveEvents sKey = do
            threadDelay 10
            tryTakeMVar sKey
        isQuited :: Maybe Events -> Bool
        isQuited (Just Quit) = True
        isQuited _           = False
        activateEvent f mE = case mE of
            Just Quit -> return ()
            Just x    -> f x
            _         -> return ()

repeatActionForEachTimimg :: (Rational,(Int,Int)) -> (MVar Events -> IO AppState) -> IO ()
repeatActionForEachTimimg info act = do
    tState <- TC.countTiming fps
    (tState,tId) <- TC.countTiming fps
    (eState,eId) <- waitForEvents
    repeatCore tState eState
    killThread eId
    killThread tId
    where
        (fps,size) = info
        repeatCore tS eS = do
            _ <- takeMVar tS
            aS <- act eS
            case aS of
                Dead   -> return ()
                Living -> repeatCore tS eS

