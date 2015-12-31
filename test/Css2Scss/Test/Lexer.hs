module Css2Scss.Test.Lexer (run) where


import Data.List
import Data.Either
import Test.Hspec
import Text.ParserCombinators.Parsec

import Css2Scss.Css.Lexer as L


run :: IO ()
run = hspec $ do
    describe "Tests for Lexer" $ do
        it "Test for all hex digits" $ do
            let parseResult = map (\x -> parse L.h "test" [x]) "0123456789abcdefABCDEF"
            all (isRight) parseResult

        it "Test for wrong hex digits" $ do
            let parseResult = map (\x -> parse L.h "test" [x]) "-+`GHIjkl"
            all (isLeft) parseResult

        it "Test for non ascii" $ do
            let parseResult = map (\x -> parse L.nonascii "test" [x]) "\o240\o10000\o4177777"
            all (isRight) parseResult

        it "Test for wrong non ascii" $ do
            let parseResult = map (\x -> parse L.nonascii "test" [x]) "\o0\o239"
            all (isLeft) parseResult

        it "Test for unicode" $ do
            let parseResult = map (\x -> parse L.unicode "test" x) ["\\4cecCd", "\\ccc "]
            all (isRight) parseResult

        it "Test for wrong unicode" $ do
            let parseResult = map (\x -> parse L.unicode "test" x) ["\\zz", "ccc "]
            all (isLeft) parseResult

        it "Test for escape" $ do
            let parseResult = map (\x -> parse L.escape "test" x) ["\\ccc\t", "\\-", "\\\o241"]
            all (isRight) parseResult

        it "Test for wrong escape" $ do
            let parseResult = map (\x -> parse L.escape "test" x) ["ccc", "\\+"]
            all (isLeft) parseResult

        it "Test for nmstart" $ do
            let parseResult = map (\x -> parse L.nmstart "test" x) ["a", "\o241", "\\~"]
            all (isRight) parseResult

        it "Test for wrong nmstart" $ do
            let parseResult = map (\x -> parse L.nmstart "test" x) ["-", "\o239", "\\+"]
            all (isLeft) parseResult

        it "Test for nmchar" $ do
            let parseResult = map (\x -> parse L.nmchar "test" x) ["-", "1", "\o241", "\\-"]
            all (isRight) parseResult

        it "Test for wrong nmchar" $ do
            let parseResult = map (\x -> parse L.nmchar "test" x) ["+", "\o239", "\\%"]
            all (isLeft) parseResult

        it "Test for string1" $ do
            let parseResult = map (\x -> parse L.string1 "test" x) ["\"\t\"", "\"\t\\\r\"", "\"\o241\o242\'\""]
            all (isRight) parseResult

        it "Test for wrong string1" $ do
            let parseResult = map (\x -> parse L.string1 "test" x) ["\"abc\"", "\"\ta\""]
            all (isLeft) parseResult

        it "Test for string2" $ do
            let parseResult = map (\x -> parse L.string2 "test" x) ["\'\t\'", "\'\t\\\r\'", "\'\o241\o242\"\'"]
            all (isRight) parseResult

        it "Test for wrong string2" $ do
            let parseResult = map (\x -> parse L.string2 "test" x) ["\'abc\'", "\'\ta\'"]
            all (isLeft) parseResult

        it "Test for ident" $ do
            let parseResult = map (\x -> parse L.ident "test" x) ["-a", "-\o241\o242ab", "ident" ]
            all (isRight) parseResult

        it "Test for wrong ident" $ do
            let parseResult = map (\x -> parse L.ident "test" x) ["_a", "-_"]
            all (isLeft) parseResult

        it "Test for name" $ do
            let parseResult = map (\x -> parse L.name "test" x) ["-a0", "-", "\o241" ]
            all (isRight) parseResult

        it "Test for wrong name" $ do
            let parseResult = map (\x -> parse L.name "test" x) ["_a", ""]
            all (isLeft) parseResult

        it "Test for num" $ do
            let parseResult = map (\x -> parse L.num "test" x) [".42", "3.14", "666" ]
            all (isRight) parseResult

        it "Test for wrong num" $ do
            let parseResult = map (\x -> parse L.num "test" x) ["-4", ".a", ".a"]
            all (isLeft) parseResult

        it "Test for nl" $ do
            let parseResult = map (\x -> parse L.nl "test" x) ["\n", "\r", "\f", "\r\n" ]
            all (isRight) parseResult

        it "Test for wrong nl" $ do
            let parseResult = map (\x -> parse L.string1 "test" x) ["a", "\o200", "\t"]
            all (isLeft) parseResult
