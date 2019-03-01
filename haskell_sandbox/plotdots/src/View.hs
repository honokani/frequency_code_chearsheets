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


initializeView size s = do
    clearScreen
    setCursorPosition 1 1
    putStrLn "command : ESC -> quit"
    printInput size s

startView size = initializeView size "start."

endView size   = initializeView size "end."

--printInput :: (Show a) => a -> IO ()
printInput size s = do
    setCursorPosition (snd size) 1
    putStrLn $ ">> " ++ show s


initialMap :: (Int,Int) -> IM.IntMap (IM.IntMap Char)
initialMap (w,h) = IM.fromList $ zip [0..(h-1)] $
    repeat $ IM.fromList $ zip [0..(w-1)] (repeat '.')

mapToLines :: IM.IntMap (IM.IntMap Char) -> [String]
mapToLines = foldr (\x -> ((:) $ map snd.IM.toList $ x)) []

