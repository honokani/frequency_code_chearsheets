{-# Language MultiWayIf #-}

module Lib
    ( activateApp
    ) where

import           Data.Ratio
import           Control.Concurrent.MVar
import           Control.Monad
import           System.IO
import           Control.Monad.State.Lazy           (runStateT)
-- my modules
import           Events                             (Events(..))
import qualified Events.Controller        as EvCtrl
import           Keyboard                           (KeyToggle(..),KeyKinds(..),Options(..),Dirs(..))
import           Keyboard.Controller                (startKeyWaiting)
import qualified Timing                   as Time
import           View                               (startView, endView, printInput)

fps   = 30 % 1 :: Rational
width = 80     :: Int
hight = 20     :: Int


activateApp :: IO ()
activateApp = if
    | width <= 0 || hight <= 0 -> return ()
    | otherwise -> do
        hSetBuffering stdin NoBuffering
        hSetEcho stdin False
        runAct mainAction


--runAct :: (Events -> IO ()) -> IO ()
runAct action = do
    startView size
    EvCtrl.repeatActUntilQuit fps action
    endView size
    where
        size = (width,hight)


--mainAction :: (KeyEff KeyKinds) -> IO ()
mainAction e = case e of
    k -> printInput size $ show k
    where
        size = (width,hight)

