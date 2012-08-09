module Main where

import System.Environment

import Text.Lye.Interactive
import Text.Lye.Types

testFile :: String -> IO ()
testFile s = do
    exprs <- test s
    let music = Music exprs
    putStrLn $ show music
    return ()

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> do
            putStrLn "Need file."
            return ()
        filepath : _ -> testFile filepath
