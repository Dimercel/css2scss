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
    , _S
    , _CDO
    , _CDC
    , _INCLUDES
    , _DASHMATCH
    , _STRING
    , _IDENT
    , _HASH
    , _IMPORT_SYM
    , _PAGE_SYM
    , _MEDIA_SYM
    , _FONT_FACE_SYM
    , _CHARSET_SYM
    , _NAMESPACE_SYM
    , _IMPORTANT_SYM
    , _EMS
    , _EXS
    , _LENGTH
    , _ANGLE
    , _TIME
    , _FREQ
    , _DIMEN
    , _PERCENTAGE
    , _NUMBER
    , _URI
    , _FUNCTION
    , _UNICODERANGE
    , _STATIC
    ) where

import Text.ParserCombinators.Parsec
import Text.Parsec.Char

data Token = S String
           | Cdo String
           | Cdc String
           | Includes String
           | Dashmatch String
           | String' String
           | Ident String
           | Hash String
           | ImportSym String
           | PageSym String
           | MediaSym String
           | FontFaceSym String
           | CharsetSym String
           | NamespaceSym String
           | ImportantSym String
           | Ems String
           | Exs String
           | Length String
           | Angle String
           | Time String
           | Freq String
           | Dimen String
           | Percentage String
           | Number String
           | Uri String
           | Function String
           | UnicodeRange String
           | Static String
           deriving (Eq, Show)


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
                res <- sequence [string "\\",
                                 count 1 (oneOf $ "-~" ++ ['\o240'..'\o4177777'])]
                return $ concat res
        <?> "escape"

nmstart :: Parser String
nmstart = do
        count 1 (oneOf $ ['a'..'z'] ++ ['A'..'Z'])
        <|> nonascii
        <|> escape
        <?> "nmstart"

nmchar :: Parser String
nmchar = do
        count 1 (oneOf $ "-" ++ ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'])
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
            <|> nmchar
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
            res <- sequence [many (oneOf ['0'..'9']), string ".",
                             many1 (oneOf ['0'..'9'])]
            return $ concat res
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

_S :: Parser Token
_S = do
        s <- many1 $ (oneOf " \t\r\n\f")
        return $ S s

_CDO :: Parser Token
_CDO = do
        string "<!--"
        return $ Cdo "<!--"

_CDC :: Parser Token
_CDC = do
        string "-->"
        return $ Cdc "-->"

_INCLUDES :: Parser Token
_INCLUDES = do
        string "~="
        return $ Includes "~="

_DASHMATCH :: Parser Token
_DASHMATCH = do
        string "|="
        return $ Dashmatch "|="

_STRING ::Parser Token
_STRING = do
        s <- _string
        return $ String' s

_IDENT :: Parser Token
_IDENT = do
        i <- ident
        return $ Ident i

_HASH :: Parser Token
_HASH = do
        string "#"
        hashname <- name
        return $ Hash (concat ["#", hashname])

_IMPORT_SYM :: Parser Token
_IMPORT_SYM = do
        string "@import"
        return $ ImportSym "@import"

_PAGE_SYM :: Parser Token
_PAGE_SYM = do
        string "@page"
        return $ PageSym "@page"

_MEDIA_SYM :: Parser Token
_MEDIA_SYM = do
        string "@media"
        return $ MediaSym "@media"

_FONT_FACE_SYM :: Parser Token
_FONT_FACE_SYM = do
        string "@font-face"
        return $ FontFaceSym "@font-face"

_CHARSET_SYM :: Parser Token
_CHARSET_SYM = do
        string "@charset"
        return $ CharsetSym "@charset"

_NAMESPACE_SYM :: Parser Token
_NAMESPACE_SYM = do
        string "@namespace"
        return $ NamespaceSym "@namespace"

_IMPORTANT_SYM :: Parser Token
_IMPORTANT_SYM = do
        char '!'
        space <- w
        string "important"
        return $ ImportantSym (concat ["!", space, "important"])

_EMS :: Parser Token
_EMS = do
        n <- num
        string "em"
        return $ Ems (concat [n, "em"])

_EXS :: Parser Token
_EXS = do
        n <- num
        string "ex"
        return $ Exs (concat [n, "em"])

_LENGTH :: Parser Token
_LENGTH = do
        n <- num
        u <- string "px"
            <|> string "cm"
            <|> string "mm"
            <|> string "in"
            <|> string "pt"
            <|> string "pc"
        return $ Length (concat [n,u])

_ANGLE :: Parser Token
_ANGLE = do
        n <- num
        u <- string "deg"
            <|> string "rad"
            <|> string "grad"
        return $ Angle (concat [n,u])

_TIME :: Parser Token
_TIME = do
        n <- num
        u <- string "ms"
            <|> string "s"
        return $ Time (concat [n,u])


_FREQ :: Parser Token
_FREQ = do
        n <- num
        u <- string "Hz"
            <|> string "kHz"
        return $ Freq (concat [n,u])

_DIMEN :: Parser Token
_DIMEN = do
        n <- num
        i <- ident
        return $ Dimen (concat [n, i])

_PERCENTAGE :: Parser Token
_PERCENTAGE = do
        n <- num
        char '%'
        return $ Percentage (concat [n, "%"])

_NUMBER :: Parser Token
_NUMBER = do
        n <- num
        return $ Number n

_URI :: Parser Token
_URI = do
        string "url(\""
        sp1 <- w
        s <- do
                _string
                <|> url
        sp2 <- w
        string "\")"
        return $ Uri (concat ["url(\"", sp1, s, sp2, "\")"])

_FUNCTION :: Parser Token
_FUNCTION = do
        i <- ident
        char '('
        return $ Function (concat [i, "("])

_UNICODERANGE :: Parser Token
_UNICODERANGE = do
        string "U+"
        start <- do
            try (count 6 h)
            <|> many1 h
        char '-'
        end <- do
            try (count 6 h)
            <|> many1 h
        return $ UnicodeRange (concat ["U+", start, "-", end])

_STATIC :: String -> Parser Token
_STATIC s = do
        str <- string s
        return $ Static str
