module Lib
    ( someFunc
    ) where

import Control.Applicative

someFunc :: IO ()
someFunc = do
    print =<< getN_and_StrLines


-- "10 20 aaa"    ["10", "20", "aaa"]
getWords :: IO [String]
getWords = do
    ws <- words <$> getLine
    return ws

-- # input    -> outout
-- "10 20 30"    [10, 20, 30]
getNumbers :: (Num a, Read a) => IO [a]
getNumbers = do
    ws <- map read.words <$> getLine
    return ws

-- # input -> output
-- "2"        [[12, 34],[567]]
-- "12 34"
-- "567"
getN_and_StrLines :: IO [[String]]
getN_and_StrLines = do
    n <- read.head.words <$> getLine
    l <- mapM (\_ -> getWords) [1..n]
    return l

-- # input -> output
-- "2"        [[12, 34],[567]]
-- "12 34"
-- "567"
getN_and_NumLines :: (Num a, Read a, Enum a, Show a) => IO [[a]]
getN_and_NumLines = do
    n <- read.head.words <$> getLine
    l <- mapM (\_ -> getNumbers) [1..n]
    return l

