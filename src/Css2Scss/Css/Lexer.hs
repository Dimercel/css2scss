module Css2Scss.Css.Lexer
    ( Token (..)
    , h
    , nonascii
    , unicode
    , escape
    , nmstart
    , nmchar
    ) where

import Text.ParserCombinators.Parsec
import Text.Parsec.Char

data Token = H String | Nonascii String deriving (Eq, Show)

h :: Parser Char
h = hexDigit <?> "h"

nonascii :: Parser String
nonascii = do
        count 1 (satisfy (\x -> x >= '\o240' && x <= '\o4177777'))
        <?> "nonascii"

unicode :: Parser String
unicode = do
        initial <- count 1 (oneOf "\\")
        hex <- try (count 6 h)
            <|> many1 h
        end <- try (count 1 (oneOf " \t\r\n\f"))
        return $ concat [initial, hex, end]
        <?> "unicode"

escape :: Parser String
escape = do
        try unicode
        <|> do
                initial <- count 1 (oneOf "\\")
                symbol <- count 1 (oneOf $ "-~" ++ ['\o240'..'\o4177777'])
                return $ concat [initial, symbol]
        <?> "escape"

nmstart :: Parser String
nmstart = do
        count 1 (oneOf ['a'..'z'])
        <|> nonascii
        <|> escape
        <?> "nmstart"

nmchar :: Parser String
nmchar = do
        count 1 (oneOf $ "-" ++ ['0'..'9'] ++ ['a'..'z'])
        <|> nonascii
        <|> escape
        <?> "nmchar"
