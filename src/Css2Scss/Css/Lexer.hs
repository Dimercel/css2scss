module Css2Scss.Css.Lexer
    ( Token (..)
    , h
    ) where

import Text.ParserCombinators.Parsec

data Token = H String | Nonascii String deriving (Eq, Show)

h :: Parser Token
h = do
    hex <- many (oneOf $ ['a'..'f'] ++ ['A'..'F'] ++ ['0'..'9'])
    return $ H hex

nonascii :: Parser Token
nonascii = do
        s <- many (oneOf ['\200'..'\377'])
        return $ Nonascii s
