{-# Language MultiWayIf #-}

module Utils where

import           Control.Concurrent      (forkIO,ThreadId,myThreadId,killThread)
import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad

-- WIP
--withForkedIO :: (MVar a -> IO ()) -> IO (MVar a)
withForkedIO act f = do
    state <- newEmptyMVar
    id <- forkIO $ f state
    act state
    killThread id

