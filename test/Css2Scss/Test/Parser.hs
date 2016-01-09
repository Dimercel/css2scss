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
            let parseResult = map (\x -> parse P.prio "test" x) ["margin@ 0 auto;", "",
                                                                 "margin : 0 auto;"]
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
            let parseResult = map (\x -> parse P.property "test" x) ["123", "#prop", ""]
            all (isLeft) parseResult

        it "Test for pseudo" $ do
            let parseResult = map (\x -> parse P.pseudo "test" x) [":nth-child(2n)",
                    ":after", ":not([controls])"]
            all (isRight) parseResult

        it "Wrong test for pseudo" $ do
            let parseResult = map (\x -> parse P.pseudo "test" x) ["after", ":#after", ""]
            all (isLeft) parseResult

        it "Test for attrib" $ do
            let parseResult = map (\x -> parse P.attrib "test" x) ["[type=button]"]
            all (isRight) parseResult

        it "Wrong test for attrib" $ do
            let parseResult = map (\x -> parse P.attrib "test" x) ["[type^=button]",
                                                                   "[type$=button]", ""]
            all (isLeft) parseResult

        it "Test for element_name" $ do
            let parseResult = map (\x -> parse P.element_name "test" x) ["body", "*", "html"]
            all (isRight) parseResult

        it "Wrong test for element_name" $ do
            let parseResult = map (\x -> parse P.element_name "test" x) ["+test", ""]
            all (isLeft) parseResult

        it "Test for _class" $ do
            let parseResult = map (\x -> parse P._class "test" x) [".class-name", ".class"]
            all (isRight) parseResult

        it "Wrong test for _class" $ do
            let parseResult = map (\x -> parse P._class "test" x) ["class", ".#class", ""]
            all (isLeft) parseResult

        it "Test for simple_selector" $ do
            let parseResult = map (\x -> parse P.simple_selector "test" x) [".class[type=button]",
                                                                            "[href=url]"]
            all (isRight) parseResult

        it "Wrong test for simple_selector" $ do
            let parseResult = map (\x -> parse P.simple_selector "test" x) [".#class"]
            all (isLeft) parseResult

        it "Test for combinator" $ do
            let parseResult = map (\x -> parse P.combinator "test" x) ["+ ", "+", "> ", ">", ""]
            all (isRight) parseResult

        it "Test for selector" $ do
            let parseResult = map (\x -> parse P.selector "test" x) ["button > input", "a > p"]
            all (isRight) parseResult

        it "Test for ruleset" $ do
            let parseResult = map (\x -> parse P.ruleset "test" x) ["textarea {margin: 0; font-family: 'Times New Roman'; color: inherit;}",
                 "input[type=checkbox], select {padding: .35em .625em .75em; margin: 0 2px; border: 1px solid #c0c0c0;}"]
            all (isRight) parseResult

        it "Test for font_face" $ do
            let parseResult = map (\x -> parse P.font_face "test" x) ["@font-face { font-family: 'Glyphicons Halflings';}"]
            all (isRight) parseResult

        it "Test for pseudo_page" $ do
            let parseResult = map (\x -> parse P.pseudo_page "test" x) [":first", ":left", ":right"]
            all (isRight) parseResult

        it "Wrong test for pseudo_page" $ do
            let parseResult = map (\x -> parse P.pseudo_page "test" x) ["first", ""]
            all (isLeft) parseResult

        it "Test for page" $ do
            let parseResult = map (\x -> parse P.page "test" x) ["@page :left { margin: 1cm 3cm 1cm 1.5cm; }"]
            all (isRight) parseResult

        it "Test for medium" $ do
            let parseResult = map (\x -> parse P.medium "test" x) ["print ", "print", "screen"]
            all (isRight) parseResult

        it "Wrong test for medium" $ do
            let parseResult = map (\x -> parse P.medium "test" x) ["#print", ""]
            all (isLeft) parseResult

        it "Test for media" $ do
            let parseResult = map (\x -> parse P.media "test" x) ["@media print { .lead { font-size: 21px; } }",
                                                          "@media print, screen { .lead { font-size: 21px; } }"]
            all (isRight) parseResult

        it "Test for namespace_prefix" $ do
            let parseResult = map (\x -> parse P.namespace_prefix "test" x) ["svg ", "Q", "lq"]
            all (isRight) parseResult

        it "Wrong test for namespace_prefix" $ do
            let parseResult = map (\x -> parse P.namespace_prefix "test" x) ["#svg", ""]
            all (isLeft) parseResult

        it "Test for namespace" $ do
            let parseResult = map (\x -> parse P.namespace "test" x) ["@namespace svg url(http://www.w3.org/2000/svg);",
                                                                      "@namespace url(http://www.w3.org/1999/xhtml);"]
            all (isRight) parseResult

        it "Wrong test for namespace" $ do
            let parseResult = map (\x -> parse P.namespace "test" x) ["namespace svg", "@namespace svg;"]
            all (isLeft) parseResult

        it "Test for _import" $ do
            let parseResult = map (\x -> parse P._import "test" x) ["@import \"/style/main.css\" screen;",
                                                                      "@import \"/style/palm.css\" handheld, print;"]
            all (isRight) parseResult

        it "Wrong test for _import" $ do
            let parseResult = map (\x -> parse P._import "test" x) ["import svg", "@import \"/style/main.css\" screen"]
            all (isLeft) parseResult
