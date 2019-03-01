{-# Language MultiWayIf #-}

module Keyboard.Controller where

import           System.IO
import           Data.Char               (chr, ord)
import           Data.Maybe              (fromMaybe)
import qualified Data.HashMap as HM
import           Control.Monad           (forM_,forever)
import           Control.Concurrent      (forkIO,threadDelay,ThreadId,killThread)
import           Control.Concurrent.MVar
-- my modules
import           Keyboard                (KeyToggle(..),KeyKinds(..),Options(..),Dirs(..))


startKeyWaiting :: IO (MVar (KeyToggle KeyKinds), ThreadId)
startKeyWaiting = do
    kStatus <- newEmptyMVar :: IO (MVar (KeyToggle KeyKinds))
    id <- forkIO $ forever $ do
        threadDelay 10
        monitorKeyInput kStatus
    return (kStatus, id)

lag :: Int
lag = 160

monitorKeyInput :: MVar (KeyToggle KeyKinds) -> IO Bool
monitorKeyInput mv = do
    k <- getKey
    tryPutMVar mv $ Pushing k
    checkKeepPushing k
    where
        candidateChunks = map fst keysFollowedEsc
        getKey = realizeKey =<< getChar
        realizeKey '\ESC' = do
            b <- hWaitForInput stdin lag
            if b then
                findKeyUsingEsc 0 candidateChunks
            else
                pure Escape
        realizeKey c =
            pure $ findKey [c]
        checkKeepPushing oK = do
            b <- hWaitForInput stdin lag
            if b then do
                nK <- getKey
                if nK == oK then
                    checkKeepPushing oK
                else do
                    tryPutMVar mv $ Released oK
                    tryPutMVar mv $ Pushing nK
                    checkKeepPushing nK
            else
                tryPutMVar mv $ Released oK
        findKeyUsingEsc i []     = pure Undef
        findKeyUsingEsc i [e]    = do
            forM_ [0..(length e - i - 1)] $ \_ -> getChar
            pure . findKey . ((:)'\ESC') $ e
        findKeyUsingEsc i escMap = do
            nC <- getChar
            findKeyUsingEsc (i+1) . filter ((==nC) . (!!i)) $ escMap


findKey :: String -> KeyKinds
findKey [c] = fromMaybe Undef $ HM.lookup c singleKeyMap
findKey s   = fromMaybe Undef $ HM.lookup s (HM.union multiKeyMapEsc multiKeyMapWinRN)


singleKeyMap = HM.fromList $ alphNumMark ++ specialKey
    where
        [base, end] = map ord [' ', '~']
        alphNumMark = [ (c, Chara c) | n <- [0..(end - base)], let c = chr $ base + n]
        specialKey  = [ ('\n'   , Return)
                      , ('\r'   , Return)
                      , ('\ESC' , Escape)
                      , (' '    , Space)
                      , ('\t'   , Tab)
                      , ('\b'   , Backspace)
                      , ('\DEL' , Delete)
                      ]


multiKeyMapWinRN = HM.fromList $ [ ("\r\n", Return) ]
multiKeyMapEsc = HM.fromList $ map appendEcs $ keysFollowedEsc
    where
        appendEcs (x,y) = ('\ESC':x, y)


keysFollowedEsc = arrow ++ func ++ specialKey
arrow =  arN ++ arS ++ arC
    where
        numToChr n = chr $ ord 'A' + n
        arN = [ ("["   ++[numToChr n], Arrow $ toEnum n) | n<-[0..3]]
        arS = [ ("[1;2"++[numToChr n], Mod Sht (Arrow $ toEnum n)) | n<-[0..3]]
        arC = [ ("[1;5"++[numToChr n], Mod Ctr (Arrow $ toEnum n)) | n<-[0..3]]
func = [ ("OP"  , Func  1)
       , ("OQ"  , Func  2)
       , ("OR"  , Func  3)
       , ("OS"  , Func  4)
       , ("[15~", Func  5)
       , ("[17~", Func  6)
       , ("[18~", Func  7)
       , ("[19~", Func  8)
       , ("[20~", Func  9)
       , ("[21~", Func 10)
       , ("[23~", Func 11)
       , ("[24~", Func 12)
       , ("[25~", Func 13)
       , ("[26~", Func 14)
       , ("[28~", Func 15)
       ]
specialKey = [ ("[H" , Home)
             , ("[F" , End)
             , ("[1~", Home)
             , ("[2~", Insert)
             , ("[3~", Delete)
             , ("[4~", End)
             , ("[5~", PageUp)
             , ("[6~", PageDn)
             ]

