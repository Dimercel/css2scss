module Css2Scss.Css.Lexer
    ( Token (..)
    , h
    , nonascii
    ) where

import Text.ParserCombinators.Parsec
import Text.Parsec.Char

data Token = H String | Nonascii String deriving (Eq, Show)

h :: Parser Token
h = do
    hex <- many hexDigit
    return $ H hex

nonascii :: Parser Token
nonascii = do
        s <- many (oneOf ['\200'..'\377'])
        return $ Nonascii s
