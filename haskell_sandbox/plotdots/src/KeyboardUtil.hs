{-# Language MultiWayIf #-}
module KeyboardUtil where

import           System.IO
import qualified Data.HashMap as HM
import           Data.Char          (chr, ord)


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

findKey [c] = case HM.lookup c singleKeyMap of
    Just k  -> k
    Nothing -> Undef
findKey s = case HM.lookup s multiKeyMap of
    Just k  -> k
    Nothing -> Undef

singleKeyMap = HM.fromList $ alphNumMark ++ specialKey
    where
        [base, end] = map ord [' ', '~']
        alphNumMark = [ (c, Chara c) | n <- [0..(end - base)], let c = chr $ base + n]
        specialKey = [ ('\n'   , Return)
                     , ('\r'   , Return)
                     , ('\ESC' , Escape)
                     , (' '    , Space)
                     , ('\t'   , Tab)
                     , ('\b'   , Backspace)
                     , ('\DEL' , Delete)
                     ]

multiKeyMap = HM.fromList $ arrow ++ func ++ specialKey
    where
        arrow = arN ++ arS ++ arC
            where
                numToChr n = chr $ ord 'A' + n
                arN = [ ("\ESC["   ++[numToChr n], Arrow $ toEnum n) | n<-[0..3]]
                arS = [ ("\ESC[1;2"++[numToChr n], Mod Sht (Arrow $ toEnum n)) | n<-[0..3]]
                arC = [ ("\ESC[1;5"++[numToChr n], Mod Ctr (Arrow $ toEnum n)) | n<-[0..3]]
        func = [ ("\ESCOP"  , Func 1)
               , ("\ESCOQ"  , Func 2)
               , ("\ESCOR"  , Func 3)
               , ("\ESCOS"  , Func 4)
               , ("\ESC[15~", Func 5)
               , ("\ESC[17~", Func 6)
               , ("\ESC[18~", Func 7)
               , ("\ESC[19~", Func 8)
               , ("\ESC[20~", Func 9)
               , ("\ESC[21~", Func 10)
               , ("\ESC[23~", Func 11)
               , ("\ESC[24~", Func 12)
               , ("\ESC[25~", Func 13)
               , ("\ESC[26~", Func 14)
               , ("\ESC[28~", Func 15)
               ]
        specialKey = [ ("\r\n"   , Return)
                     , ("\ESC[H" , Home)
                     , ("\ESC[F" , End)
                     , ("\ESC[1~", Home)
                     , ("\ESC[2~", Insert)
                     , ("\ESC[3~", Delete)
                     , ("\ESC[4~", End)
                     , ("\ESC[5~", PageUp)
                     , ("\ESC[6~", PageDn)
                     ]

