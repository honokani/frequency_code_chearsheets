{-# Language MultiWayIf #-}

module Lib
    ( activateApp
    ) where

import qualified Control.Concurrent      as CC (forkIO, threadDelay, killThread)
import           Control.Concurrent.MVar
import           Control.Monad
import           System.IO
-- my modules
import           InputController
import           TimingController

activateApp = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    run mainAction

run action = do
    (sKey,kid) <- waitForInput
    --(sTiming,tid) <- countTiming
    action sKey
    CC.killThread kid

mainAction sKey = do
    printKB $ "start."
    untilLiving sKey $ return ()
    printKB $ "end."

