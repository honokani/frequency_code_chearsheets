module Events where

-- my modules
import Keyboard       (KeyKinds(..))

data Events = Quit
            | KB KeyKinds
            deriving (Show, Eq)

