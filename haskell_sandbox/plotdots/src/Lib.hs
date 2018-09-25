{-# Language MultiWayIf #-}

module Lib
    ( activateApp
    ) where

import           Control.Concurrent             (forkIO, threadDelay, killThread)
import           Control.Concurrent.MVar
import           Control.Monad
import           System.IO
import           Control.Monad.State.Lazy       (runStateT)
-- my modules
import           Events                         (Events(..))
import qualified Events.Controller        as EC
import qualified TimingController         as TC


fps = 30
activateApp :: IO ()
activateApp = do
    runAct mainAction


runAct :: IO () -> IO ()
runAct action = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    EC.printKB "start."
    (eStat,eId) <- EC.waitForEvents
    (tStat,tId) <- TC.countTiming fps
    (`runStateT` (eStat,tStat)) $ EC.untilQuit action
    killThread tId
    killThread eId
    EC.printKB "end."


mainAction :: IO ()
mainAction = do
    EC.printKB "hi"

