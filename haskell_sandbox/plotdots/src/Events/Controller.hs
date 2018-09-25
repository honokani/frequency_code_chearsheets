{-# Language MultiWayIf #-}

module Events.Controller where

import           Control.Concurrent            (forkIO,threadDelay,ThreadId)
import           Control.Concurrent.MVar
import           Control.Monad                 (forM_)
import           Control.Monad.State.Lazy       (StateT, runStateT, get, put, lift)
import           System.Console.ANSI
-- my modules
import           Events                        (Events(..))
import           Keyboard.Controller           (getEventFromKeyboard)
import qualified TimingController         as TC


checkLiving (Just Quit) = False
checkLiving _           = True

activateEvent e act = case e of
    Just Quit   -> return ()
    Just (KB k) -> act k
    _           -> return ()

waitForEvents :: IO (MVar Events, ThreadId)
waitForEvents = do
    threadDelay 10
    waitForInputKeyboad

recieveEvents :: MVar Events -> IO (Maybe Events)
recieveEvents sKey = do
    threadDelay 10
    tryTakeMVar sKey

waitForInputKeyboad :: IO (MVar Events, ThreadId)
waitForInputKeyboad = do
    kStatus <- newEmptyMVar :: IO (MVar Events)
    id <- forkIO $ forM_ (repeat ()) $ \_ -> do
        setKeyboardEvent kStatus
    return (kStatus, id)

setKeyboardEvent :: MVar Events -> IO ()
setKeyboardEvent mv = getEventFromKeyboard >>= putMVar mv

printKB :: (Show a) => a -> IO ()
printKB s = do
    clearScreen
    setCursorPosition　1 1
    putStrLn "command - q: quit, "
    setCursorPosition　2 1
    print s



type AppStates = (MVar Events, MVar TC.Tick)

untilQuit :: IO () -> StateT AppStates IO ()
untilQuit act = do
    (eStat, tStat) <- get
    e <- lift $ recieveEvents eStat
    if checkLiving e then do
        lift $ activateEvent e printKB
        lift $ act
        untilQuit act
    else do
        lift $ printKB "quited."

