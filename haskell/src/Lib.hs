module Lib
    ( someFunc
    ) where

import Control.Applicative
import Control.Monad

someFunc :: IO ()
someFunc = do
    print =<< getData_and_Nnums

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
-- # input     -> output
-- "2"            [12, 34]
-- "12 34 567"
getN_and_Nums :: (Num a, Read a, Enum a, Show a) => IO [a]
getN_and_Nums = do
    n <- read.head.words <$> getLine
    take n.<$> getNumbers
    where
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
    replicateM l getWords
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
    replicateM l getNumbers
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
    replicateM l $ take n <$> getWords
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
    replicateM l $ take n <$> getNumbers
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
    replicateM l $ take n.head <$> getWords
    where
        mapT2 :: (a -> b) -> (a,a) -> (b,b)
        mapT2 f (a1,a2) = (f a1, f a2)
        listToT2s :: [a] -> [(a,a)]
        listToT2s [] = []
        listToT2s (x:[]) = []
        listToT2s (x:y:zs) = (x,y) : listToT2s zs
        getWords :: IO [String]
        getWords = words <$> getLine

------------------------------------------------------------------------
-- # input     -> output
-- "100 2"        (100, [12, 34])
-- "12 34 555"
getData_and_Nnums :: (Num a, Read a, Enum a, Show a) => IO (Int, [a])
getData_and_Nnums = do
    (d, n) <- mapT2 read.head.listToT2s.words <$> getLine
    xs <- take n <$> getNumbers
    return (d, xs)
    where
        mapT2 :: (a -> b) -> (a,a) -> (b,b)
        mapT2 f (a1,a2) = (f a1, f a2)
        listToT2s :: [a] -> [(a,a)]
        listToT2s [] = []
        listToT2s (x:[]) = []
        listToT2s (x:y:zs) = (x,y) : listToT2s zs
        getNumbers :: (Num a, Read a) => IO [a]
        getNumbers = map read.words <$> getLine

------------------------------------------------------------------------

