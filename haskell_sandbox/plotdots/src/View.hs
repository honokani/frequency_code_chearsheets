{-# Language MultiWayIf #-}

module View
    ( startView
    , endView
    , printInput
    ) where

import qualified Data.IntMap.Strict  as IM
import           Control.Monad
import           System.IO
import           System.Console.ANSI



initializeView s = do
    clearScreen
    setCursorPosition　1 1
    putStrLn "command : ESC -> quit"
    printInput s

startView = initializeView "start."

endView = initializeView "end."

printInput :: (Show a) => a -> IO ()
printInput s = do
    setCursorPosition　2 1
    putStrLn $ ">> " ++ show s


initialMap :: (Int,Int) -> IM.IntMap (IM.IntMap Char)
initialMap (w,h) = IM.fromList $ zip [0..(h-1)] $
    repeat $ IM.fromList $ zip [0..(w-1)] (repeat '.')

mapToLines :: IM.IntMap (IM.IntMap Char) -> [String]
mapToLines = foldr (\x -> ((:) $ map snd.IM.toList $ x)) []

