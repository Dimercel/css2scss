module Css2Scss.Test.Lexer (run) where


import Test.Hspec
import Text.ParserCombinators.Parsec

import Css2Scss.Css.Lexer as L


run :: IO ()
run = hspec $ do
    describe "Tests for Lexer" $ do
        it "Test for hex digits" $ do
            parse L.h "expr" "0" == Right '0'
