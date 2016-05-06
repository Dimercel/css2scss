module Css2Scss.Test.Scss (run) where

import Data.Label
import Test.QuickCheck
import Test.Hspec
import qualified Css2Scss.Scss as S

prop_isEmptyRule rule = (length (get S.props rule) /= 0) ==> not $ S.isEmptyRule rule

prop_isNotEmptyRule rule = (length (get S.props rule) /= 0) ==> S.isNotEmptyRule rule

run :: IO ()
run = hspec $ do
    describe "Test for SCSS functions" $ do
        it "Test for isEmptyRule" $ do
            quickCheck (prop_isEmptyRule :: S.Rule -> Property)

        it "Test for IsNotEmptyRule" $ do
            quickCheck (prop_isNotEmptyRule :: S.Rule -> Property)

