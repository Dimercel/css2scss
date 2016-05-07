module Css2Scss.Test.Css (run) where

import Test.Hspec
import Test.QuickCheck
import qualified Css2Scss.Css as C
import Css2Scss.Test.Lexer (TokenId)
import Css2Scss.Css.Lexer (Token, TokenId)


prop_getTokBefore t tokens = case t `elem` tokens of
    True  -> property $ (length $ C.getTokBefore t tokens) <= length tokens
    False -> property $ C.getTokBefore t tokens == []

prop_getTokAfter t tokens = case t `elem` tokens of
    True  -> property $ (length $ C.getTokAfter t tokens) < length tokens
    False -> property $ C.getTokAfter t tokens == []

run :: IO ()
run = hspec $ do
    describe "Test for Css functions" $ do
        it "Test for getTokBefore" $ do
            quickCheck (prop_getTokBefore :: Token -> [Token] -> Property)

        it "Test for getTokAfter" $ do
            quickCheck (prop_getTokAfter :: Token -> [Token] -> Property)
