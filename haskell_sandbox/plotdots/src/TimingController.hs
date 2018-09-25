{-# Language MultiWayIf #-}

module TimingController where

import           Control.Concurrent      (ThreadId, forkIO, threadDelay, killThread)
import           Control.Concurrent.MVar
import           Control.Monad

type Tick = ()

countTiming :: Int -> IO (MVar Tick, ThreadId)
countTiming fps = do
    tStatus <- newMVar ()
    id <- forkIO $ forM_ (repeat ()) $ \_ -> do
        tryPutMVar tStatus ()
        threadDelay $ div 100000 fps
    return (tStatus, id)

