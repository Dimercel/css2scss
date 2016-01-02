module Css2Scss.Css.Lexer
    ( Token (..)
    , h
    , nonascii
    , unicode
    , escape
    , nmstart
    , nmchar
    , string1
    , string2
    , ident
    , name
    , num
    , nl
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
        initial <- string "\\"
        hex <- try (count 6 h)
            <|> many1 h
        end <- option "" (count 1 (oneOf " \t\r\n\f"))
        return $ concat [initial, hex, end]
        <?> "unicode"

escape :: Parser String
escape = do
        try unicode
        <|> do
                initial <- string "\\"
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

string1 :: Parser String
string1 = do
        string "\""
        content <- many $ do
            count 1 (oneOf "\t !#$%&(-~")
            <|> do
                    string "\\"
                    nl_cont <- nl
                    return $ concat ["\\", nl_cont]
            <|> string "\'"
            <|> nonascii
            <|> escape
        string "\""
        return $ concat ["\"", concat content, "\""]
        <?> "string1"

string2 :: Parser String
string2 = do
        string "\'"
        content <- many $ do
            count 1 (oneOf "\t !#$%&(-~")
            <|> do
                    string "\\"
                    nl_cont <- nl
                    return $ concat ["\\", nl_cont]
            <|> string "\""
            <|> nonascii
            <|> escape
        string "\'"
        return $ concat ["\'", concat content, "\'"]
        <?> "string2"

ident :: Parser String
ident = do
        minus <- option "" (string "-")
        first <- nmstart
        any <- many nmchar
        return $ concat [minus, first, concat any]
        <?> "ident"

name :: Parser String
name = do
        name_sym <- many1 nmchar
        return $ concat name_sym
        <?> "name"

num :: Parser String
num = do
        many1 (oneOf ['0'..'9'])
        <|> do
            int <- many (oneOf ['0'..'9'])
            string "."
            fraction <- many1 (oneOf ['0'..'9'])
            return $ concat [int, ".", fraction]
        <?> "num"

nl :: Parser String
nl = do
        count 1 (oneOf "\n\r\f")
        <|> string "\r\n"
        <?> "nl"
