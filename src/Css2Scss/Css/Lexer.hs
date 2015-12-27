module Css2Scss.Css.Lexer
    ( Token (..)
    , h
    , nonascii
    , unicode
    ) where

import Text.ParserCombinators.Parsec
import Text.Parsec.Char

data Token = H String | Nonascii String deriving (Eq, Show)

h :: Parser Char
h = hexDigit

nonascii :: Parser String
nonascii = do
        count 1 (satisfy (\x -> x >= '\o240' && x <= '\o4177777'))

unicode :: Parser String
unicode = do
        initial <- count 1 (oneOf "\\")
        hex <- try (count 6 h)
            <|> many1 h
        end <- try (count 1 (oneOf " \t\r\n\f"))
        return $ concat [initial, hex, end]
