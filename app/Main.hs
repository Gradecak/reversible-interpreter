module Main where

import Interpreter
import System.IO

main :: IO ()
main =


parseFile :: String -> IO [Statement]
parseFile path = do
    f <- readFile path
    return $ read f
