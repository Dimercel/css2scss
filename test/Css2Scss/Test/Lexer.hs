module Css2Scss.Test.Lexer (run) where


import Data.List
import Data.Either
import Test.Hspec
import Text.ParserCombinators.Parsec

import Css2Scss.Css.Lexer as L

rightTest :: (Parser String) -> [String] -> Bool
rightTest f l = all (==True) (map (\x -> Right x == parse f "test" x) l)

wrongTest :: (Parser String) -> [String] -> Bool
wrongTest f l = all (==True) (map (\x -> isLeft $ parse f "test" x) l)

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
            rightTest (L.nonascii) ["\o240","\o10000","\o4177777"]

        it "Test for wrong non ascii" $ do
            wrongTest (L.nonascii) ["\o0","\o239"]

        it "Test for unicode" $ do
            rightTest (L.unicode) ["\\4cecCd", "\\ccc "]

        it "Test for wrong unicode" $ do
            wrongTest (L.unicode) ["\\zz", "ccc "]

        it "Test for escape" $ do
            rightTest (L.escape) ["\\ccc\t", "\\-", "\\\o241"]

        it "Test for wrong escape" $ do
            wrongTest (L.escape) ["ccc", "\\\r"]

        it "Test for nmstart" $ do
            rightTest (L.nmstart) ["a", "\o241", "\\~"]

        it "Test for wrong nmstart" $ do
            wrongTest (L.nmstart) ["-", "\o239"]

        it "Test for nmchar" $ do
            rightTest (L.nmchar) ["-", "1", "\o241", "\\-", "Z"]

        it "Test for wrong nmchar" $ do
            wrongTest (L.nmchar) ["+", "\o239"]

        it "Test for string1" $ do
            rightTest (L.string1) ["\"\t\"", "\"\t\\\r\"", "\"test string\""]

        it "Test for wrong string1" $ do
            wrongTest (L.string1) ["\'test\'", "\"test\'"]

        it "Test for string2" $ do
            rightTest (L.string2) ["\'string\'","\'\t\'", "\'\t\\\r\'", "\'test string\'"]

        it "Test for wrong string2" $ do
            wrongTest (L.string2) ["\"str\'", "\'str\""]

        it "Test for ident" $ do
            rightTest (L.ident) ["-a", "-\o241\o242ab", "identTest" ]

        it "Test for wrong ident" $ do
            wrongTest (L.ident) ["#a", "-#"]

        it "Test for name" $ do
            rightTest (L.name) ["-a0", "-", "\o241", "TestName"]

        it "Test for wrong name" $ do
            wrongTest (L.name) [" test", "#test"]

        it "Test for num" $ do
            rightTest (L.num) [".42", "3.14", "666"]

        it "Test for wrong num" $ do
            wrongTest (L.num) ["-4", ".a", ".a"]

        it "Test for string" $ do
            rightTest (L._string) ["\"\t\'\o240\"", "\'!\"\o240\'"]

        it "Test for wrong string" $ do
            wrongTest (L._string) ["\'!!!\"", "\"!!!\'"]

        it "Test for url" $ do
            rightTest (L.url) ["!\o240\o241\\22\r"]

        {-it "Test for wrong url" $ do-}
            {-wrongTest (L.url) ["\\url"]-}

        it "Test for w" $ do
            rightTest (L.w) [" \t", "\f"]

        it "Test for nl" $ do
            rightTest (L.nl) ["\n", "\r", "\f", "\r\n"]

        it "Test for wrong nl" $ do
            wrongTest (L.nl) ["a", "\o200", "\t"]

        it "Test for sym_d" $ do
            rightTest (L.sym_d) ["d", "\\0044", "\\064", "\\064\r\n"]

        it "Test for wrong sym_d" $ do
            wrongTest (L.sym_d) ["a", "\\0000044", "\\077"]

        it "Test for sym_e" $ do
            rightTest (L.sym_e) ["e", "\\0045", "\\065", "\\065\r\n"]

        it "Test for wrong sym_e" $ do
            wrongTest (L.sym_e) ["a", "\\0000045", "\\077"]

        it "Test for sym_n" $ do
            rightTest (L.sym_n) ["n", "\\004e", "\\06e", "\\06e\r\n", "\\n"]

        it "Test for wrong sym_n" $ do
            wrongTest (L.sym_n) ["a", "\\000004e", "\\077"]

        it "Test for sym_o" $ do
            rightTest (L.sym_o) ["o", "\\004f", "\\06f", "\\06f\r\n", "\\o"]

        it "Test for wrong sym_o" $ do
            wrongTest (L.sym_o) ["a", "\\000004f", "\\077"]

        it "Test for sym_t" $ do
            rightTest (L.sym_t) ["t", "\\0054", "\\074", "\\074\r\n", "\\t"]

        it "Test for wrong sym_t" $ do
            wrongTest (L.sym_t) ["a", "\\0000054", "\\077"]

        it "Test for sym_v" $ do
            rightTest (L.sym_v) ["v", "\\0058", "\\078", "\\078\r\n", "\\v"]

        it "Test for wrong sym_v" $ do
            wrongTest (L.sym_v) ["a", "\\0000058", "\\077"]
