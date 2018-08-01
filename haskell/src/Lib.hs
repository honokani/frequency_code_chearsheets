module Lib
    ( someFunc
    ) where

import Control.Applicative
import Control.Monad

someFunc :: IO ()
someFunc = do
    print =<< getNI_and_InfoWithNums

------------------------------------------------------------------------
-- # input     -> outout
-- "10 20 aaa"    ["10", "20", "aaa"]
getWords :: IO [String]
getWords = words <$> getLine

-- # input    -> outout
-- "10 20 30"    [10, 20, 30]
getNumbers :: (Num a, Read a, Enum a) => IO [a]
getNumbers = map read.words <$> getLine

------------------------------------------------------------------------
-- # input     -> output
-- "2"            ["aaa", "bb"]
-- "aaa bb cc"
getN_and_Strs :: IO [String]
getN_and_Strs = do
    n <- read.head.words <$> getLine
    take n <$> getWords
    where
        getWords :: IO [String]
        getWords = words <$> getLine

-- # input     -> output
-- "2"            [12, 34]
-- "12 34 567"
getN_and_Nums :: (Num a, Read a, Enum a) => IO [a]
getN_and_Nums = do
    n <- read.head.words <$> getLine
    take n <$> getNumbers
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
getL_and_Nums :: (Num a, Read a, Enum a) => IO [[a]]
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
    (l, n) <- mapT2 read.listToT2.take 2.words <$> getLine
    replicateM l $ take n <$> getWords
    where
        listToT2 :: [a] -> (a,a)
        listToT2 (x:y:_) = (x,y)
        mapT2 :: (a -> b) -> (a,a) -> (b,b)
        mapT2 f (a1,a2) = (f a1, f a2)
        getWords :: IO [String]
        getWords = words <$> getLine

-- # input       -> output
-- "2 2"            [[12, 34],[567,89]]
-- "12 34 555"
-- "567 89 0000"
getLN_and_Nums :: (Num a, Read a, Enum a) => IO [[a]]
getLN_and_Nums = do
    (l, n) <- mapT2 read.listToT2.take 2.words <$> getLine
    replicateM l $ take n <$> getNumbers
    where
        listToT2 :: [a] -> (a,a)
        listToT2 (x:y:_) = (x,y)
        mapT2 :: (a -> b) -> (a,a) -> (b,b)
        mapT2 f (a1,a2) = (f a1, f a2)
        getNumbers :: (Num a, Read a) => IO [a]
        getNumbers = map read.words <$> getLine

-- # input -> output
-- "2 1"      ['a','o'] (=="ao")
-- "abc"
-- "opqr"
getLN_and_Chrs :: IO [String]
getLN_and_Chrs = do
    (l, n) <- mapT2 read.listToT2.take 2.words <$> getLine
    replicateM l $ take n.head <$> getWords
    where
        listToT2 :: [a] -> (a,a)
        listToT2 (x:y:_) = (x,y)
        mapT2 :: (a -> b) -> (a,a) -> (b,b)
        mapT2 f (a1,a2) = (f a1, f a2)
        getWords :: IO [String]
        getWords = words <$> getLine

------------------------------------------------------------------------
-- # input     -> output
-- "2 100"        (["ab", "cd"], "100")
-- "ab cd eee"
getNI_and_InfoWithStrs:: IO ([String], String)
getNI_and_InfoWithStrs = do
    (n, i) <- mapT2fst read.listToT2.take 2.words <$> getLine
    xs <- take n <$> getWords
    return (xs, i)
    where
        listToT2 :: [a] -> (a,a)
        listToT2 (x:y:_) = (x,y)
        mapT2fst :: (a -> b) -> (a,a) -> (b,a)
        mapT2fst f (a1,a2) = (f a1, a2)
        getWords :: IO [String]
        getWords = words <$> getLine

-- # input     -> output
-- "2 100"        ([12, 34], "100")
-- "12 34 555"
getNI_and_InfoWithNums:: (Num a, Read a, Enum a) => IO ([a], String)
getNI_and_InfoWithNums = do
    (n, i) <- mapT2fst read.listToT2.take 2.words <$> getLine
    xs <- take n <$> getNumbers
    return (xs, i)
    where
        listToT2 :: [a] -> (a,a)
        listToT2 (x:y:_) = (x,y)
        mapT2fst :: (a -> b) -> (a,a) -> (b,a)
        mapT2fst f (a1,a2) = (f a1, a2)
        getNumbers :: (Num a, Read a) => IO [a]
        getNumbers = map read.words <$> getLine

------------------------------------------------------------------------

