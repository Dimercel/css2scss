module Main where

import System.Environment
import System.IO
import Text.ParserCombinators.Parsec
import Css2Scss.Css.Parser
import Css2Scss.Css.Lexer
import Css2Scss.Css as C
import Css2Scss.Scss.Converter


main :: IO ()
main = do
        args <- getArgs
        handle <- openFile (head args) ReadMode
        contents <- hGetContents handle
        let tokens = do
                parse stylesheet "" (preprocessor contents)
        {-print test'-}
        case tokens of
            Right t -> print $ buildSCSSRulesets $ C.buildRulesets $ splitOnBaseLexems t
            Left err -> print err
        hClose handle
