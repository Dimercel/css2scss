module Css2Scss.Test.Css (run) where

import Test.QuickCheck
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

testGetTokBefore t ts = C.getTokBefore t ts /= [] ==> t `notElem` ts
    ==> (length $ fst $ span (/=t) ts) == (length $ C.getTokBefore t ts)

testGetTokAfter t ts
    | t `elem` ts = t `elem` ts
        ==> (length $ C.getTokAfter t ts) < length ts
        ==> (head $ C.getTokAfter t ts) /= t
    | t `notElem` ts = t `notElem` ts 
        ==> C.getTokAfter t ts == []

run :: IO ()
run = do
        quickCheck (testGetTokBefore :: Token -> [Token] -> Property)
        quickCheck (testGetTokAfter :: Token -> [Token] -> Property)
