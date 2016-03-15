module Css2Scss.Test.Css (run) where

import Test.QuickCheck
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Random
import qualified Css2Scss.Css as C
import Css2Scss.Test.Lexer (TokenId)
import Css2Scss.Css.Lexer (Token(..), TokenId(..))
import Control.Applicative
import Control.Monad

listWithToken:: Token -> Gen [Token]
listWithToken t = do
        genList <- (t :) <$> (arbitrary :: Gen [Token])
        result <- shuffle genList
        return result

listWithOutToken:: Token -> Gen [Token]
listWithOutToken t = (filter (/= t)) <$> (arbitrary :: Gen [Token])

prop_getTokBefore t = t `elem` withT
        ==> (length $ C.getTokBefore t withT) <= length withT
        ==> C.getTokBefore t withOutT == []
            where withT = unGen (listWithToken t) (mkQCGen 0) 0
                  withOutT = unGen (listWithOutToken t) (mkQCGen 0) 0

prop_getTokAfter t = t `elem` withT
        ==> (length $ C.getTokAfter t withT) < length withT
        ==> C.getTokAfter t withOutT == []
            where withT = unGen (listWithToken t) (mkQCGen 0) 0
                  withOutT = unGen (listWithOutToken t) (mkQCGen 0) 0

run :: IO ()
run = do
        quickCheck (prop_getTokBefore :: Token -> Property)
        quickCheck (prop_getTokAfter :: Token -> Property)
