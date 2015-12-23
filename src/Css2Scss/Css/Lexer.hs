module Css2Scss.Css.Lexer
    ( Token (..)
    , h
    ) where

import Text.ParserCombinators.Parsec

data Token = H String deriving (Eq, Show)

h :: Parser Token
h = do
    hex <- many (oneOf "0123456789abcdef")
    return $ H hex
