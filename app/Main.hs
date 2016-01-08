module Main where

import System.Environment
import System.IO
import Text.ParserCombinators.Parsec
import Css2Scss.Css.Parser
import Css2Scss.Css.Lexer as L


main :: IO ()
main = do
        args <- getArgs
        handle <- openFile (head args) ReadMode
        contents <- hGetContents handle
        parseTest stylesheet contents
        hClose handle
