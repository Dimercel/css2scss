module Css2Scss.Test.Css (run) where

import Test.QuickCheck
import qualified Css2Scss.Css as C
import Css2Scss.Test.Lexer (TokenId)
import Css2Scss.Css.Lexer (Token(..), TokenId(..))
import Control.Applicative
import Control.Monad

listWithToken:: Token -> Gen [Token]
listWithToken t = shuffle =<< (t :) <$> (arbitrary :: Gen [Token])

testGetTokBefore t ts = C.getTokBefore t ts /= [] ==> t `notElem` ts
    ==> (length $ fst $ span (/=t) ts) == (length $ C.getTokBefore t ts)

run :: IO ()
run = do
        quickCheck (testGetTokBefore :: Token -> [Token] -> Property)
