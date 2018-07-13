module Lib
    ( someFunc
    ) where

import Control.Applicative

someFunc :: IO ()
someFunc = do
    print =<< getN_and_Nlines

-- # input -> output
-- "2"         [[12, 34],[567]]
-- "12   34"
-- " 567   "
getN_and_Nlines :: (Num a, Read a, Enum a, Show a) => IO [[a]]
getN_and_Nlines = do
    n <- read.head.words <$> getLine
    l <- mapM (\_ -> getNumberList) [1..n]
    return l


-- # input
-- "10 20 30"
-- # outout
-- [10, 20, 30]
getNumberList :: (Num a, Read a) => IO [a]
getNumberList = do
    ws <- map read.words <$> getLine
    return ws

