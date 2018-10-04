{-# Language MultiWayIf #-}

module Lib
    ( activateApp
    ) where

import           Data.Ratio
import           Control.Concurrent.MVar
import           Control.Monad
import           System.IO
import           Control.Monad.State.Lazy       (runStateT)
-- my modules
import           Events                         (Events(..))
import qualified Events.Controller        as EC
import qualified Timing                   as TC
import           View


fps   = 30 % 1 :: Rational
width = 80     :: Int
hight = 20     :: Int

activateApp :: IO ()
activateApp = runAct mainAction

runAct :: (Events -> IO ()) -> IO ()
runAct action = if
    | width <= 0 || hight <= 0 -> return ()
    | otherwise -> do
        hSetBuffering stdin NoBuffering
        hSetEcho stdin False
        startView
        EC.repeatActionForEachTimimg info $ EC.untilQuit action
        endView
    where
        info = (fps,(width,hight))

mainAction :: Events -> IO ()
mainAction e = do
    keyboardEvents e

keyboardEvents e = case e of
    (KB k) -> printInput k

