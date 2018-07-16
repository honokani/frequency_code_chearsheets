module Lib
    ( someFunc
    ) where

import Control.Applicative

someFunc :: IO ()
someFunc = do
    print =<< getNL_and_Chrs

------------------------------------------------------------------------
-- "10 20 aaa"    ["10", "20", "aaa"]
getWords :: IO [String]
getWords = words <$> getLine

-- # input    -> outout
-- "10 20 30"    [10, 20, 30]
getNumbers :: (Num a, Read a) => IO [a]
getNumbers = map read.words <$> getLine

------------------------------------------------------------------------
-- # input -> output
-- "2"        [["abc", "zzzz"],["opqr"]]
-- "abc zzzz"
-- "opqr"
getN_and_Strs :: IO [[String]]
getN_and_Strs = do
    n <- read.head.words <$> getLine
    mapM (\_ -> getWords) [1..n]
    where
        getWords :: IO [String]
        getWords = words <$> getLine

-- # input -> output
-- "2"        [[12, 34],[567]]
-- "12 34"
-- "567"
getN_and_Nums :: (Num a, Read a, Enum a, Show a) => IO [[a]]
getN_and_Nums = do
    n <- read.head.words <$> getLine
    mapM (\_ -> getNumbers) [1..n]
    where
        getNumbers :: (Num a, Read a) => IO [a]
        getNumbers = map read.words <$> getLine

------------------------------------------------------------------------
-- # input      -> output
-- "2 1"           [["abc"],["opqr"]]
-- "abc zzzz"
-- "opqr 11 lm"
getNL_and_Strs :: IO [[String]]
getNL_and_Strs = do
    (n, l) <- mapTuple read.head.listToTuples.words <$> getLine
    mapM (\_ -> take l <$> getWords) [1..n]
    where
        mapTuple :: (a -> b) -> (a,a) -> (b,b)
        mapTuple f (a1,a2) = (f a1, f a2)
        listToTuples :: [a] -> [(a,a)]
        listToTuples [] = []
        listToTuples (x:[]) = []
        listToTuples (x:y:zs) = (x,y) : listToTuples zs
        getWords :: IO [String]
        getWords = words <$> getLine

-- # input       -> output
-- "2 2"            [[12, 34],[567,89]]
-- "12 34 555"
-- "567 89 0000"
getNL_and_Nums :: (Num a, Read a, Enum a, Show a) => IO [[a]]
getNL_and_Nums = do
    (n, l) <- mapTuple read.head.listToTuples.words <$> getLine
    mapM (\_ -> take l <$> getNumbers) [1..n]
    where
        mapTuple :: (a -> b) -> (a,a) -> (b,b)
        mapTuple f (a1,a2) = (f a1, f a2)
        listToTuples :: [a] -> [(a,a)]
        listToTuples [] = []
        listToTuples (x:[]) = []
        listToTuples (x:y:zs) = (x,y) : listToTuples zs
        getNumbers :: (Num a, Read a) => IO [a]
        getNumbers = map read.words <$> getLine

-- # input -> output
-- "2 1"      ["a","o"]
-- "abc"
-- "opqr"
getNL_and_Chrs :: IO [String]
getNL_and_Chrs = do
    (n, l) <- mapTuple read.head.listToTuples.words <$> getLine
    mapM (\_ -> take l.head <$> getWords) [1..n]
    where
        mapTuple :: (a -> b) -> (a,a) -> (b,b)
        mapTuple f (a1,a2) = (f a1, f a2)
        listToTuples :: [a] -> [(a,a)]
        listToTuples [] = []
        listToTuples (x:[]) = []
        listToTuples (x:y:zs) = (x,y) : listToTuples zs

------------------------------------------------------------------------
