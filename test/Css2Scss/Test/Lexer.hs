module Css2Scss.Test.Lexer (run) where


import Data.List
import Test.Hspec
import Text.ParserCombinators.Parsec

import Css2Scss.Css.Lexer as L

isRight :: Either a b -> Bool
isRight x = case x of
                Left _ -> False
                Right _ -> True

isLeft :: Either a b -> Bool
isLeft x = not $ isRight x

run :: IO ()
run = hspec $ do
    describe "Tests for Lexer" $ do
        it "Test for all hex digits" $ do
            let parseResult = foldr (\x acc -> (parse L.h "test" [x]) : acc) [] "0123456789abcdefABCDEF"
            all (isRight) parseResult

        it "Test for wrong hex digits" $ do
            let parseResult = foldr (\x acc -> (parse L.h "test" [x]) : acc) [] "-+`GHIjkl"
            all (isLeft) parseResult

        it "Test for non ascii" $ do
            let parseResult = foldr (\x acc -> (parse L.nonascii "test" [x]) : acc) [] "\o240\o10000\o4177777"
            all (isRight) parseResult

        it "Test for wrong non ascii" $ do
            let parseResult = foldr (\x acc -> (parse L.nonascii "test" [x]) : acc) [] "\o0\o239"
            all (isLeft) parseResult

        it "Test for unicode" $ do
            let parseResult = foldr (\x acc -> (parse L.unicode "test" x) : acc) [] ["\\4cecCd", "\\ccc "]
            all (isRight) parseResult

        it "Test for wrong unicode" $ do
            let parseResult = foldr (\x acc -> (parse L.unicode "test" x) : acc) [] ["\\zz", "ccc "]
            all (isLeft) parseResult

        it "Test for escape" $ do
            let parseResult = foldr (\x acc -> (parse L.escape "test" x) : acc) [] ["\\ccc\t", "\\-", "\\\o241"]
            all (isRight) parseResult

        it "Test for wrong escape" $ do
            let parseResult = foldr (\x acc -> (parse L.escape "test" x) : acc) [] ["ccc", "\\+"]
            all (isLeft) parseResult

        it "Test for nmstart" $ do
            let parseResult = foldr (\x acc -> (parse L.nmstart "test" x) : acc) [] ["a", "\o241", "\\~"]
            all (isRight) parseResult

        it "Test for wrong nmstart" $ do
            let parseResult = foldr (\x acc -> (parse L.nmstart "test" x) : acc) [] ["-", "\o239", "\\+"]
            all (isLeft) parseResult

        it "Test for nmchar" $ do
            let parseResult = foldr (\x acc -> (parse L.nmchar "test" x) : acc) [] ["-", "1", "\o241", "\\-"]
            all (isRight) parseResult

        it "Test for wrong nmchar" $ do
            let parseResult = foldr (\x acc -> (parse L.nmchar "test" x) : acc) [] ["+", "\o239", "\\%"]
            all (isLeft) parseResult
