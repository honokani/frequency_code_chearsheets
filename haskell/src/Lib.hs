module Lib
    ( someFunc
    ) where

import Control.Applicative

someFunc :: IO ()
someFunc = do
    print =<< getLN_and_Chrs

------------------------------------------------------------------------
-- # input     -> outout
-- "10 20 aaa"    ["10", "20", "aaa"]
getWords :: IO [String]
getWords = words <$> getLine

-- # input    -> outout
-- "10 20 30"    [10, 20, 30]
getNumbers :: (Num a, Read a) => IO [a]
getNumbers = map read.words <$> getLine

------------------------------------------------------------------------
-- # input    -> output
-- "2"           [["abc", "zzzz"],["opqr"]]
-- "abc zzzz"
-- "opqr"
getL_and_Strs :: IO [[String]]
getL_and_Strs = do
    l <- read.head.words <$> getLine
    mapM (\_ -> getWords) [1..l]
    where
        getWords :: IO [String]
        getWords = words <$> getLine

-- # input -> output
-- "2"        [[12, 34],[567]]
-- "12 34"
-- "567"
getL_and_Nums :: (Num a, Read a, Enum a, Show a) => IO [[a]]
getL_and_Nums = do
    l <- read.head.words <$> getLine
    mapM (\_ -> getNumbers) [1..l]
    where
        getNumbers :: (Num a, Read a) => IO [a]
        getNumbers = map read.words <$> getLine

------------------------------------------------------------------------
-- # input      -> output
-- "2 1"           [["abc"],["opqr"]]
-- "abc zzzz"
-- "opqr 11 lm"
getLN_and_Strs :: IO [[String]]
getLN_and_Strs = do
    (l, n) <- mapT2 read.head.listToT2s.words <$> getLine
    mapM (\_ -> take n <$> getWords) [1..l]
    where
        mapT2 :: (a -> b) -> (a,a) -> (b,b)
        mapT2 f (a1,a2) = (f a1, f a2)
        listToT2s :: [a] -> [(a,a)]
        listToT2s [] = []
        listToT2s (x:[]) = []
        listToT2s (x:y:zs) = (x,y) : listToT2s zs
        getWords :: IO [String]
        getWords = words <$> getLine

-- # input       -> output
-- "2 2"            [[12, 34],[567,89]]
-- "12 34 555"
-- "567 89 0000"
getLN_and_Nums :: (Num a, Read a, Enum a, Show a) => IO [[a]]
getLN_and_Nums = do
    (l, n) <- mapT2 read.head.listToT2s.words <$> getLine
    mapM (\_ -> take n <$> getNumbers) [1..l]
    where
        mapT2 :: (a -> b) -> (a,a) -> (b,b)
        mapT2 f (a1,a2) = (f a1, f a2)
        listToT2s :: [a] -> [(a,a)]
        listToT2s [] = []
        listToT2s (x:[]) = []
        listToT2s (x:y:zs) = (x,y) : listToT2s zs
        getNumbers :: (Num a, Read a) => IO [a]
        getNumbers = map read.words <$> getLine

-- # input -> output
-- "2 1"      ["a","o"]
-- "abc"
-- "opqr"
getLN_and_Chrs :: IO [String]
getLN_and_Chrs = do
    (l, n) <- mapT2 read.head.listToT2s.words <$> getLine
    mapM (\_ -> take n.head <$> getWords) [1..l]
    where
        mapT2 :: (a -> b) -> (a,a) -> (b,b)
        mapT2 f (a1,a2) = (f a1, f a2)
        listToT2s :: [a] -> [(a,a)]
        listToT2s [] = []
        listToT2s (x:[]) = []
        listToT2s (x:y:zs) = (x,y) : listToT2s zs

------------------------------------------------------------------------
