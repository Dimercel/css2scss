module Css2Scss.Test.Parser (run) where

import Data.List
import Data.Either
import Test.Hspec
import Text.ParserCombinators.Parsec
import Css2Scss.Css.Parser as P
import Css2Scss.Css.Lexer as L

collectData :: Either ParseError [L.Token] -> String
collectData (Right x) = concat $ map (\x -> snd x) x
collectData _ = ""

rightTest :: (Parser [L.Token]) -> [String] -> Bool
rightTest f l = all (==True) (map (\x -> x == collectData(parse f "test" x)) l)

wrongTest :: (Parser [L.Token]) -> [String] -> Bool
wrongTest f l = all (==True) (map (\x -> isLeft $ parse f "test" x) l)

run :: IO ()
run = hspec $ do
    describe "Tests for Parser" $ do
        it "Test for hexcolor" $ do
            rightTest P.hexcolor ["#123456  ", "#zzz"]

        it "Wrong test for hexcolor" $ do
            wrongTest P.hexcolor ["123  ", "#+test"]

        it "Test for function" $ do
            rightTest P.function ["rgba(0, 0, 0, .25)","rgb(0,0,0)"]

        it "Wrong test for function" $ do
            wrongTest P.function ["#(0,0,0)", "func"]

        it "Test for term" $ do
            rightTest P.term ["-25px ", ".2em", "100%", "url(\"/url\")"]

        it "Wrong test for term" $ do
            wrongTest P.term ["&amp;"]

        it "Test for expr" $ do
            rightTest P.expr ["-25px 0 -10px 2", "solid 1px red",
                    "border .2s ease-in-out", "inline-block",
                    "inset 0 -1px 0 rgba(0, 0, 0, .15)"]

        it "Wrong test for expr" $ do
            wrongTest P.expr ["&0px 20px", "@test"]

        it "Test for prio" $ do
            rightTest P.prio ["!important", "!\timportant\n"]

        it "Wrong test for prio" $ do
            wrongTest P.prio ["important", ""]

        it "Test for declaration" $ do
            rightTest P.declaration ["margin: 1em 40px",
                    "-webkit-transition: width .6s ease",
                    "cursor: not-allowed"]

        it "Wrong test for declaration" $ do
             wrongTest P.prio ["margin@ 0 auto;", "", "margin : 0 auto;"]

        it "Test for unary_operator" $ do
            rightTest P.unary_operator ["+", "-"]

        it "Wrong test for unary_operator" $ do
            wrongTest P.unary_operator ["*", "/", ""]

        it "Test for operator" $ do
            rightTest P.operator ["/ ", ", ", ""]

        it "Test for property" $ do
            rightTest P.property ["red", "solid "]

        it "Wrong test for property" $ do
            wrongTest P.property ["123", "#prop", ""]

        it "Test for pseudo" $ do
            rightTest P.pseudo [":nth-child(2n)", ":after",
                "::-moz-focus-inner"]

        it "Wrong test for pseudo" $ do
            wrongTest P.pseudo ["after", ":#after", ""]

        it "Test for attrib" $ do
            rightTest P.attrib ["[ type = button ]", "[type^=button]",
                                "[type~=button]", "[type|=button]",
                                "[type$=button]", "[type*=button]",
                                "[ type = 'button' ]", "[type^='button']",
                                "[type~='button']", "[type|=\"button\"]",
                                "[type$=\"button\"]", "[type*=\"button\"]"]

        it "Wrong test for attrib" $ do
            wrongTest P.attrib ["[#type=button]", "[type=#button#]", ""]

        it "Test for element_name" $ do
            rightTest P.element_name ["body", "html"]

        it "Wrong test for element_name" $ do
            wrongTest P.element_name ["+test", ""]

        it "Test for _class" $ do
            rightTest P._class [".class-name", ".class"]

        it "Wrong test for _class" $ do
            wrongTest P._class ["class", ".#class", ""]

        it "Test for combinator" $ do
            rightTest P.combinator ["+ ", "+", "> ", ">", "~", "~ ", "", "  "]

        it "Test for selector" $ do
            rightTest P.selector ["button > input", "a > p",
                                  ".dropup > input.btn > .caret",
                                  "textarea"]

        it "Test for ruleset" $ do
            rightTest P.ruleset ["textarea {margin: 0; font-family: 'Times New Roman'; color: inherit;}",
                "input[type=checkbox], select {padding: .35em .625em .75em; margin: 0 2px; border: 1px solid #c0c0c0;}"]

        it "Test for font_face" $ do
            rightTest P.font_face ["@font-face { font-family: 'Glyphicons Halflings';}"]

        it "Test for pseudo_page" $ do
            rightTest P.pseudo_page [":first", ":left", ":right"]

        it "Wrong test for pseudo_page" $ do
            wrongTest P.pseudo_page ["first", ""]

        it "Test for page" $ do
            rightTest P.page ["@page :left { margin: 1cm 3cm 1cm 1.5cm; }"]

        it "Test for medium" $ do
            rightTest P.medium ["print ", "print", "screen"]

        it "Wrong test for medium" $ do
            wrongTest P.medium ["#print", ""]

        it "Test for media" $ do
            rightTest P.media ["@media print { .lead { font-size: 21px; } }",
                               "@media print, screen { .lead { font-size: 21px; } }",
                               "@media (min-width: 768px){}",
                               "@media screen and (max-width: 767px){}",
                               "@media (max-device-width: 480px) and (orientation: landscape)",
                               "@media all and (transform-3d), (-webkit-transform-3d)"
                               ]

        it "Test for namespace_prefix" $ do
            rightTest P.namespace_prefix ["svg|", "*|", "|", "svg"]

        it "Test for namespace" $ do
            rightTest P.namespace ["@namespace svg| url(http://www.w3.org/2000/svg);",
                                   "@namespace url(http://www.w3.org/1999/xhtml);"]

        it "Wrong test for namespace" $ do
            wrongTest P.namespace ["namespace svg", "@namespace svg;"]

        it "Test for _import" $ do
            rightTest P._import ["@import \"/style/main.css\" screen;",
                                 "@import \"/style/palm.css\" handheld, print;"]

        it "Wrong test for _import" $ do
            wrongTest P._import ["import svg", "@import \"/style/main.css\" screen"]

        it "Test for expresion" $ do
            rightTest P.expression ["1px solid red", "'Glyphicons Halflings'"]

        it "Wrong test for expression" $ do
            wrongTest P.expression ["\20ac", ""]

        it "Test for functional_pseudo" $ do
            rightTest P.functional_pseudo  ["nth-of-type(odd)"]

        it "Wrong test for functional_pseudo" $ do
            wrongTest P.functional_pseudo ["#test()", "test()", ""]

        it "Test for negation" $ do
            rightTest P.negation [":not([controls])", ":not(.dropdown-toggle)", ":not(:root)"]

        it "Wrong test for negation" $ do
            wrongTest P.negation ["not(.class)", ":not(.test", ""]

        it "Test for negation_arg" $ do
            rightTest P.negation_arg [".class", "[button]", ":root"]

        it "Wrong test for negation_arg" $ do
            wrongTest P.negation_arg [" test", ""]

        it "Test for universal" $ do
            rightTest P.universal ["svg|*", "*", "*|*"]

        it "Wrong test for universal" $ do
            wrongTest P.universal ["#test", "svg", ""]

        it "Test for type_selector" $ do
            rightTest P.type_selector ["*p", "svg|circle", "*|circle", "body"]

        it "Wrong test for type_selector" $ do
            wrongTest P.type_selector ["#test", "svg|", ""]

        it "Test for simple_selector_sequence" $ do
            rightTest P.simple_selector_sequence ["a:hover", "*|a.class", "audio:not([controls])",
                                                  "abbr[title]", "abbr"]

        it "Test for selectors_group" $ do
            rightTest P.selectors_group ["a.text-primary:hover, a.text-primary:focus",
               "audio:not([controls])", "[hidden],\ntemplate",
               "input[type=\"checkbox\"], input[type=radio]",
               "input[type=\"number\"]::-webkit-inner-spin-button"]
