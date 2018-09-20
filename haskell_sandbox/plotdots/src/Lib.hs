{-# Language MultiWayIf #-}

module Lib
    ( runPlotting
    ) where

import qualified Control.Concurrent      as CC (forkIO, threadDelay)
import           Control.Concurrent.MVar
import           System.Console.ANSI
import           Control.Monad

data Event = Quit
           | KB Char
           deriving (Show, Eq)

runPlotting :: IO ()
runPlotting = do
    status <- newEmptyMVar :: IO (MVar Event)
    CC.forkIO $ forM_ (cycle [0]) $ \_ -> do
        inputKeyboard status

    printKB $ "start"
    untilLiving $ recieveEvent status
    printKB $ "end"


seconds :: Int -> Int
seconds n = n*1000000


untilLiving :: IO Event -> IO ()
untilLiving act = do
    e <- act
    if checkLiving e then do
        activateEvent e
        untilLiving act
    else
        printKB $ "quited"

checkLiving Quit = False
checkLiving _    = True

activateEvent :: Event -> IO ()
activateEvent e = case e of
    Quit -> return ()
    KB c -> printKB [c]

recieveEvent :: (MVar Event) -> IO Event
recieveEvent mv = do
    CC.threadDelay 10
    takeMVar mv

inputKeyboard :: (MVar Event) -> IO ()
inputKeyboard mv = do
    CC.threadDelay 10
    l <- getLine
    if l=='q' then
        putMVar mv $ Quit
    else
        putMVar mv $ KB l

printKB s = do
    clearScreen
    setCursorPosition　1 1
    putStrLn "command - q: quit, "
    setCursorPosition　2 1
    putStrLn s

