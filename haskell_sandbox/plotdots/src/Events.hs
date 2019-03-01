module Events where

-- my modules
import Keyboard       (KeyKinds(..))

data Events = Undef
            | Quit
            | KB KeyKinds
            deriving (Show, Eq)

