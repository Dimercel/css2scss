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
    , _string
    , url
    , nl
    , w
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

_string :: Parser String
_string = do
        string1
        <|> string2
        <?> "_string"

url :: Parser String
url = do
        content <- many $ do
            count 1 (oneOf "!#$%&*-~")
            <|> nonascii
            <|> escape
        return $ concat content
        <?> "url"

w :: Parser String
w = do
        many (oneOf " \t\r\n\f")
        <?> "w"

nl :: Parser String
nl = do
        count 1 (oneOf "\n\r\f")
        <|> string "\r\n"
        <?> "nl"

range :: Parser String
range = do
        return ""
        <?> "range"

_S :: Parser String
_S = many1 $ oneOf " \t\r\n\f"

_CDO :: Parser String
_CDO = string "<!--"

_CDC :: Parser String
_CDC = string "-->"

_INCLUDES :: Parser String
_INCLUDES = string "~="

_DASHMATCH :: Parser String
_DASHMATCH = string "|="

_STRING ::Parser String
_STRING = _string

_IDENT :: Parser String
_IDENT = ident

_HASH :: Parser String
_HASH = do
        string "#"
        hashname <- name
        return $ concat ["#", hashname]

_IMPORT_SYM :: Parser String
_IMPORT_SYM = string "@import"

_PAGE_SYM :: Parser String
_PAGE_SYM = string "@page"

_MEDIA_SYM :: Parser String
_MEDIA_SYM = string "@media"

_FONT_FACE_SYM :: Parser String
_FONT_FACE_SYM = string "@font-face"

_CHARSET_SYM :: Parser String
_CHARSET_SYM = string "@charset"

_NAMESPACE_SYM :: Parser String
_NAMESPACE_SYM = string "@namespace"

_IMPORTANT_SYM :: Parser String
_IMPORTANT_SYM = do
        char '!'
        space <- w
        string "important"
        return $ concat ["!", space, "important"]

_EMS :: Parser String
_EMS = do
        n <- num
        string "em"
        return $ concat [n, "em"]

_EXS :: Parser String
_EXS = do
        n <- num
        string "ex"
        return $ concat [n, "em"]

_LENGTH :: Parser String
_LENGTH = do
        n <- num
        u <- string "px"
            <|> string "cm"
            <|> string "mm"
            <|> string "in"
            <|> string "pt"
            <|> string "pc"
        return $ concat [n,u]

_ANGLE :: Parser String
_ANGLE = do
        n <- num
        u <- string "deg"
            <|> string "rad"
            <|> string "grad"
        return $ concat [n,u]

_TIME :: Parser String
_TIME = do
        n <- num
        u <- string "ms"
            <|> string "s"
        return $ concat [n,u]


_FREQ :: Parser String
_FREQ = do
        n <- num
        u <- string "Hz"
            <|> string "kHz"
        return $ concat [n,u]

_DIMEN :: Parser String
_DIMEN = do
        n <- num
        i <- ident
        return $ concat [n, i]

_PERCENTAGE :: Parser String
_PERCENTAGE = do
        n <- num
        char '%'
        return $ concat [n, "%"]

_NUMBER :: Parser String
_NUMBER = num

_URI :: Parser String
_URI = do
        string "url(\""
        sp1 <- w
        s <- do
                _string
                <|> url
        sp2 <- w
        string "\")"
        return $ concat ["url(\"", sp1, s, sp2, "\")"]

_UNICODERANGE :: Parser String
_UNICODERANGE = do
        string "U+"
        start <- do
            try (count 6 h)
            <|> many1 h
        char '-'
        end <- do
            try (count 6 h)
            <|> many1 h
        return $ concat ["U+", start, "-", end]
