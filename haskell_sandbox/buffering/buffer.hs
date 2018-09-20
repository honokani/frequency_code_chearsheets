{-# Language MultiWayIf #-}
module Main where

import System.IO

main = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    input

prompt :: String -> IO ()
prompt p = do
    putStr p
    --hFlush stdout

input = do
    prompt "> "
    c <- getChar
    b <- hReady stdin
    putStrLn [c]
    if  | c == 'q' -> do
            putStrLn [c]
            putStrLn "quit"
        | c == '\ESC' -> do
            if  | not b -> do
                    putStrLn "ESC"
                | otherwise -> do
                    hiddenC1 <- getChar
                    if  | hiddenC1 == '[' -> do
                            hidden2 <- getChar
                            if  | hidden2 == 'A' ->
                                    putStrLn "UP"
                                | hidden2 == 'B' ->
                                    putStrLn "Down"
                                | hidden2 == 'C' ->
                                    putStrLn "Right"
                                | hidden2 == 'D' ->
                                    putStrLn "Left"
                        | otherwise ->
                            putStrLn "otherkey"
            input
        | otherwise -> do
            putStrLn [c]
            hFlush stdout
            input

