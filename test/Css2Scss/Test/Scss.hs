module Css2Scss.Test.Scss (run) where

import Test.QuickCheck
import Test.Hspec
import qualified Css2Scss.Scss as S

instance Arbitrary S.Property where
        arbitrary = do
            name <- arbitrary
            val <- arbitrary
            return $ S.Property name val

instance Arbitrary S.Rule where
        arbitrary = do
            selector <- arbitrary
            props <- arbitrary
            return $ S.Rule selector props

prop_isEmptyRule rule@(S.Rule sel props) = (length props /= 0) ==> not $ S.isEmptyRule rule

prop_isNotEmptyRule rule@(S.Rule sel props) = (length props /= 0) ==> S.isNotEmptyRule rule

run :: IO ()
run = hspec $ do
    describe "Test for SCSS functions" $ do
        it "Test for isEmptyRule" $ do
            quickCheck (prop_isEmptyRule :: S.Rule -> Property)

        it "Test for IsNotEmptyRule" $ do
            quickCheck (prop_isNotEmptyRule :: S.Rule -> Property)

