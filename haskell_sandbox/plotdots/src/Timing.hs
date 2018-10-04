{-# Language MultiWayIf #-}

module Timing where

import           Data.Ratio
import           Control.Concurrent           (ThreadId, forkIO, threadDelay, killThread)
import           Control.Concurrent.MVar
import           Control.Monad
import qualified Utils                   as U

type Tick = ()

oneSec :: Rational
oneSec = 100000

countTiming :: Rational -> IO (MVar Tick, ThreadId)
countTiming fps = do
    tStatus <- newMVar ()
    id <- forkIO $ forever $ do
        tryPutMVar tStatus ()
        threadDelay timeSpan
    return (tStatus, id)
    where
        timeSpan = ceiling.fromRational.(/) oneSec $ fps

-- WIP:
runTick fps tState = do
    tryPutMVar tState ()
    threadDelay timeSpan
    where
        timeSpan = ceiling.fromRational.(/) oneSec $ fps

wrapTick fps act = do
    U.withForkedIO (runTick fps) act

