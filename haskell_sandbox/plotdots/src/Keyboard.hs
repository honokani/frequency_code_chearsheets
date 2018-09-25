{-# Language MultiWayIf #-}
module Keyboard where

import           System.IO


data Options = Sht
             | Ctr
             | Alt
             deriving (Show,Eq)

data Dirs = Up
          | Dn
          | Ri
          | Lf
          deriving (Show,Eq,Enum)

data KeyKinds = Undef
              | Chara Char
              | Arrow Dirs
              | Func Int
              | Return
              | Escape
              | Space
              | Tab
              | Backspace
              | Home
              | Insert
              | Delete
              | End
              | PageUp
              | PageDn
              | Mod Options KeyKinds
              deriving (Show, Eq)

