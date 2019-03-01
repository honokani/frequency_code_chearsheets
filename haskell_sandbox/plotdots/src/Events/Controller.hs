{-# Language MultiWayIf #-}

module Events.Controller where

import           Control.Concurrent              (forkIO,threadDelay,ThreadId,killThread)
import           Control.Concurrent.MVar
import           Control.Monad                   (forever)
import           Control.Monad.State.Lazy        (StateT, runStateT, get, put, lift)
-- my modules
import           Events                          (Events(..))
import           Keyboard                        (KeyToggle(..),KeyKinds(..),Options(..),Dirs(..))
import           Keyboard.Controller             (startKeyWaiting)
import qualified Timing                  as Time
import           View


data AppState = Living
              | Dead
              deriving (Show)


repeatActUntilQuit :: Rational -> ((KeyToggle KeyKinds) -> IO ()) -> IO ()
repeatActUntilQuit fps act = repeatAct fps $ untilQuit act


untilQuit :: ((KeyToggle KeyKinds) -> IO ()) -> MVar (KeyToggle KeyKinds) -> IO AppState
untilQuit act eState = do
    mayEv <- tryTakeMVar eState
    activateEvent act mayEv
    where
        activateEvent f mE = case mE of
            Nothing                          ->        return Living
            Just (Pushing  (Keyboard.Undef)) ->        return Living
            Just (Released (Keyboard.Undef)) ->        return Living
            Just (Pushing  Escape)           ->        return Dead
            Just (Released Escape)           ->        return Dead
            Just x                           -> f x >> return Living


repeatAct :: Rational -> (MVar (KeyToggle KeyKinds) -> IO AppState) -> IO ()
repeatAct fps act = do
    (tState,tId) <- Time.startCounting fps
    (eState,eId) <- startKeyWaiting
    repeatActCore tState eState
    killThread eId
    killThread tId
    where
        repeatActCore tState eState = do
            _           <- takeMVar tState
            resultActed <- act eState
            case resultActed of
                Dead   -> return ()
                Living -> repeatActCore tState eState


keyToEvent :: KeyKinds -> Events
keyToEvent k = case k of
    Escape  -> Quit
    Keyboard.Undef   -> Events.Undef
    _       -> KB k

