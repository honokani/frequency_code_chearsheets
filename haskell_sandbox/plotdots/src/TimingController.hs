{-# Language MultiWayIf #-}

module TimingController where

import qualified Control.Concurrent      as CC (forkIO, threadDelay, killThread)
import           Control.Concurrent.MVar
import           Control.Monad

countTiming fps = do
    tStatus <- newMVar ()
    id <- CC.forkIO $ forM_ (cycle [()]) $ \_ -> do
        tryPutMVar tStatus ()
        CC.threadDelay $ div 100000 fps
    return $ (tStatus, id)

