module Main where

import System.Environment
import System.IO
import Css2Scss.Css.Parser


main :: IO ()
main = do
        args <- getArgs
        handle <- openFile (head args) ReadMode
        contents <- hGetContents handle
        print(stylesheet contents)
        hClose handle
