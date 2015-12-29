module Css2Scss.Test.Lexer (run) where


import Data.List
import Test.Hspec
import Text.ParserCombinators.Parsec

import Css2Scss.Css.Lexer as L


run :: IO ()
run = hspec $ do
    describe "Tests for Lexer" $ do
        it "Test for all hex digits" $ do
            let predicate x = parse L.h "test" [x] == Right x
                in take 22 (cycle [True]) == foldl' (\acc x ->
                    (predicate x) : acc) [] "0123456789abcdefABCDEF"

        it "Test for wrong hex digits" $ do
            case parse L.h "test" "g" of
                Left _ -> True
                Right _ -> False
