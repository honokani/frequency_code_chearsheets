module Lib
    ( someFunc
    ) where

import Control.Applicative

someFunc :: IO ()
someFunc = do
    print =<< inputNumList

inputNumList :: (Num a, Read a) => IO [a]
inputNumList = do
    ws <- map read.words <$> getLine
    return ws

