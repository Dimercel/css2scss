module Main where

import System.Environment
import System.IO
import Text.ParserCombinators.Parsec
import Css2Scss.Css.Parser
import Css2Scss.Css.Lexer


main :: IO ()
main = do
        args <- getArgs
        handle <- openFile (head args) ReadMode
        contents <- hGetContents handle
        let tokens = do
                parse stylesheet "" (preprocessor contents)
        print $ tokens
        hClose handle
