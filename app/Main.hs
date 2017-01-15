module Main where

import Interpreter
import System.IO
import System.Environment (getArgs)

main :: IO ()
main = do
    src:_ <- getArgs
    program <- parseFile src
    interpret program

parseFile :: String -> IO Statement
parseFile path = do
    f <- readFile path
    return $ read f
