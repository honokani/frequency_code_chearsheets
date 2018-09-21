{-# Language MultiWayIf #-}

module InputController where

import qualified Control.Concurrent      as CC (forkIO, threadDelay, killThread)
import           Control.Concurrent.MVar
import           Control.Monad
import           System.IO
import           System.Console.ANSI
-- my modules
import qualified EventController         as EC
import           EventController               (Event(..))
import qualified KeyboardUtil            as KU

waitForInput = do
    kStatus <- newEmptyMVar :: IO (MVar Event)
    id <- CC.forkIO $ forM_ (cycle [0]) $ \_ -> do
        CC.threadDelay 10
        setInputKey kStatus
    return $ (kStatus, id)

setInputKey :: (MVar Event) -> IO ()
setInputKey mv = do
    getKeyboardInput >>= putMVar mv.EC.keyToEvent.KU.findKey

getKeyboardInput :: IO String
getKeyboardInput = do
    c <- getChar
    b <- hReady stdin
    if | b         -> (c:) <$> getKeyboardInput
       | otherwise -> return [c]

recieveInput :: (MVar Event) -> IO (Maybe Event)
recieveInput sKey = do
    CC.threadDelay 10
    tryTakeMVar sKey

untilLiving :: (MVar Event) -> IO () -> IO ()
untilLiving sKey act = do
    e <- recieveInput sKey
    if EC.checkLiving e then do
        EC.activateEvent e printKB
        act
        untilLiving sKey act
    else
        printKB $ "quited"

printKB :: (Show a) => a -> IO ()
printKB s = do
    clearScreen
    setCursorPosition　1 1
    putStrLn "command - q: quit, "
    setCursorPosition　2 1
    print s

