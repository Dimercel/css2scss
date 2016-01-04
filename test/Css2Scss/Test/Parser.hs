module Css2Scss.Test.Parser (run) where

import Data.List
import Data.Either
import Test.Hspec
import Text.ParserCombinators.Parsec
import Css2Scss.Css.Parser as P

run :: IO ()
run = hspec $ do
    describe "Tests for Parser" $ do
        it "Test for hexcolor" $ do
            let parseResult = map (\x -> parse P.hexcolor "test" x) ["#123456  ", "#zzz"]
            all (isRight) parseResult

        it "Wrong test for hexcolor" $ do
            let parseResult = map (\x -> parse P.hexcolor "test" x) ["123  ", "#+test"]
            all (isLeft) parseResult

        it "Test for function" $ do
            let parseResult = map (\x -> parse P.function "test" x) ["rgba(0, 0, 0, .25)",
                                                                     "rgb(0,0,0)"]
            all (isRight) parseResult

        it "Wrong test for function" $ do
            let parseResult = map (\x -> parse P.function "test" x) ["#(0,0,0)", "func"]
            all (isLeft) parseResult

        it "Test for term" $ do
            let parseResult = map (\x -> parse P.term "test" x) ["-25px ", ".2em", "100%",
                                                                 "url(\"/url\")"]
            all (isRight) parseResult

        it "Wrong test for term" $ do
            let parseResult = map (\x -> parse P.term "test" x) ["&amp;"]
            all (isLeft) parseResult

        it "Test for expr" $ do
            let parseResult = map (\x -> parse P.expr "test" x) ["-25px 0 -10px 2",
                    "solid 1px red", "border .2s ease-in-out", "inline-block",
                    "inset 0 -1px 0 rgba(0, 0, 0, .15)"]
            all (isRight) parseResult

        it "Wrong test for expr" $ do
            let parseResult = map (\x -> parse P.expr "test" x) ["&0px 20px", "@test"]
            all (isLeft) parseResult

        it "Test for prio" $ do
            let parseResult = map (\x -> parse P.prio "test" x) ["!important", "!\timportant\n"]
            all (isRight) parseResult

        it "Wrong test for prio" $ do
            let parseResult = map (\x -> parse P.prio "test" x) ["important", ""]
            all (isLeft) parseResult

        it "Test for declaration" $ do
            let parseResult = map (\x -> parse P.declaration "test" x) ["margin: 1em 40px;",
                    "-webkit-transition: width .6s ease;", "cursor: not-allowed;",
                    "filter: alpha(opacity=90);"]
            all (isRight) parseResult

        it "Wrong test for declaration" $ do
            let parseResult = map (\x -> parse P.prio "test" x) ["margin@ 0 auto;", "", "margin : 0 auto;"]
            all (isLeft) parseResult

        it "Test for unary_operator" $ do
            let parseResult = map (\x -> parse P.unary_operator "test" x) ["+", "-"]
            all (isRight) parseResult

        it "Wrong test for unary_operator" $ do
            let parseResult = map (\x -> parse P.unary_operator "test" x) ["*", "/", ""]
            all (isLeft) parseResult

        it "Test for operator" $ do
            let parseResult = map (\x -> parse P.operator "test" x) ["/ ", ", ", ""]
            all (isRight) parseResult

        it "Test for property" $ do
            let parseResult = map (\x -> parse P.property "test" x) ["red", "solid "]
            all (isRight) parseResult

        it "Wrong test for property" $ do
            let parseResult = map (\x -> parse P.property "test" x) ["123", "_prop", ""]
            all (isLeft) parseResult
