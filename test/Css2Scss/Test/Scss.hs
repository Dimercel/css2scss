module Css2Scss.Test.Scss (run) where

import Test.QuickCheck
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

testIsEmptyRule rule@(S.Rule sel props) = (length props /= 0) ==> not $ S.isEmptyRule rule

testIsNotEmptyRule rule@(S.Rule sel props) = (length props /= 0) ==> S.isNotEmptyRule rule

run :: IO ()
run = do
        quickCheck (testIsEmptyRule :: S.Rule -> Property)
        quickCheck (testIsNotEmptyRule :: S.Rule -> Property)

