module Main where

import System.Environment

import Text.Lye.Export
import Text.Lye.Interactive
import Text.Lye.Types
import Text.Lye.Visitor

testFile :: String -> IO ()
testFile s = do
    exprs <- exprsFromFile s
    let music = Music exprs
    putStrLn $ show music
    let cleaned = applyStages music
    putStrLn $ show cleaned
    let midi = schedule cleaned 480
    putStrLn $ show midi
    export 480 midi

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> do
            putStrLn "Need file."
            return ()
        filepath : _ -> testFile filepath
