{-# Language MultiWayIf #-}

module EventController where

-- my modules
import           KeyboardUtil                  (KeyKinds(..))

data Event = Quit
           | KB KeyKinds
           deriving (Show, Eq)

keyToEvent :: KeyKinds -> Event
keyToEvent k = case k of
    Chara c   -> if c=='q' then Quit else KB k
    otherwise -> KB k

checkLiving (Just Quit) = False
checkLiving _           = True

activateEvent e act = case e of
    Just Quit   -> return ()
    Just (KB k) -> act k
    _           -> return ()

