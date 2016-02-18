module Css2Scss.Css.Lexer
    ( Token (..)
    , TokenId(..)
    , splitOnBaseLexems
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
    , sym_d
    , sym_e
    , sym_n
    , sym_o
    , sym_t
    , sym_v
    , _S
    , _CDO
    , _CDC
    , _INCLUDES
    , _DASHMATCH
    , _PREFIXMATCH
    , _SUFFIXMATCH
    , _SUBSTRINGMATCH
    , _STRING
    , _IDENT
    , _HASH
    , _PLUS
    , _GREATER
    , _COMMA
    , _TILDE
    , _NOT
    , _ATKEYWORD
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
    , _DIMENSION
    , _NUMBER
    , _URI
    , _MEDIA_LIMIT
    , _FUNCTION
    , _UNICODERANGE
    , _STATIC
    ) where

import Text.ParserCombinators.Parsec
import Text.Parsec.Char

import Data.List
import Data.Maybe
import Control.Monad.State as ST

data TokenId = S
               | Cdo
               | Cdc
               | Includes
               | DashMatch
               | PrefixMatch
               | SuffixMatch
               | SubMatch
               | String'
               | Ident
               | Hash
               | Plus
               | Greater
               | Comma
               | Tilde
               | Not
               | AtKeyWord
               | Dimension
               | ImportSym
               | PageSym
               | MediaSym
               | FontFaceSym
               | CharsetSym
               | NamespaceSym
               | ImportantSym
               | Ems
               | Exs
               | Length
               | Angle
               | Time
               | Freq
               | Dimen
               | Percentage
               | Number
               | Uri
               | MediaLimit
               | Function
               | UnicodeRange
               | SubStringMatch
               | Static
               deriving (Eq, Show)

type Token = (TokenId, String)


_findToken :: Token -> (Token -> Token -> Bool) -> ST.State [Token] [Token]
_findToken t f = ST.state $ \x -> (take (equalElem t x) x, drop (equalElem t x) x)
    where equalElem t elems = let inx = findIndex (f t) elems
            in case isJust inx of
                   True -> 1 + (fromJust inx)
                   False -> 0

findTokenById :: Token -> ST.State [Token] [Token]
findTokenById t = _findToken t (\x y -> fst x == fst y)

findToken :: Token -> ST.State [Token] [Token]
findToken t = _findToken t (==)


findBefore :: Token -> ST.State [Token] [Token]
findBefore t = ST.state $ \x -> (take (equalElem t x) x, drop (equalElem t x) x)
    where equalElem t elems = let inx = findIndex (== t) elems
            in case isJust inx of
                   True -> (fromJust inx)
                   False -> 0

-- | Возвращает подмассив содержащий токены находящиеся 
-- между парными токенами. Учитывется случай вложенности токенов.
-- Параметры: первый парный элемент, второй парный элемент,
-- массив токенов, кол-во парных элементов(изначально (0,0))
findPair :: Token -> Token -> [Token] -> (Int, Int) -> [Token]
findPair _ _ [] _ = []
findPair l r (x:xs) pos
    | fst pos == snd pos && pos /= (0, 0) = []
    | otherwise = x : findPair l r xs (fst pos + equal l, snd pos + equal r)
        where equal s = case x == s of
                            True -> 1
                            False -> 0

findPairM :: Token -> Token -> ST.State [Token] [Token]
findPairM l r = ST.state $ \x -> (betweenPair x, drop (length $ betweenPair x) x)
    where betweenPair x = findPair l r x (0, 0)


getData :: [Token] -> String
getData x = concat $ map (\i -> snd i) x

charset_lex :: [Token] -> [Token]
charset_lex x
        | head x == (CharsetSym, "@charset") = ST.evalState (
            do
                a <- findTokenById (CharsetSym,"")
                b <- findToken (Static,";")
                return $ concat [a, b]) x
        | otherwise = []

import_lex :: [Token] -> [Token]
import_lex x
        | head x == (CharsetSym, "@import") = ST.evalState (
            do
                a <- findTokenById (ImportSym,"")
                b <- findToken (Static,";")
                return $ concat [a, b]) x
        | otherwise = []

namespace_lex :: [Token] -> [Token]
namespace_lex x
        | head x == (NamespaceSym, "@namespace") = ST.evalState (
            do
                a <- findTokenById (NamespaceSym,"")
                b <- findToken (Static,";")
                return $ concat [a, b]) x
        | otherwise = []

page_lex :: [Token] -> [Token]
page_lex x
        | head x == (PageSym, "@page") = ST.evalState (
            do
                a <- findTokenById (PageSym,"")
                b <- findToken (Static,";")
                return $ concat [a, b]) x
        | otherwise = []

font_face_lex :: [Token] -> [Token]
font_face_lex x
        | head x == (FontFaceSym, "@font-face") = ST.evalState (
            do
                a <- findTokenById (FontFaceSym,"")
                b <- findToken (Static,";")
                return $ concat [a, b]) x
        | otherwise = []

media_lex :: [Token] -> [Token]
media_lex x
        | head x == (MediaSym, "@media") = ST.evalState (
            do
                a <- findToken (MediaSym, "@media")
                b <- findBefore (Static, "{")
                c <- findPairM (Static, "{") (Static, "}")
                return $ concat [a, b, c]) x
        | otherwise = []

ruleset_lex :: [Token] -> [Token]
ruleset_lex x
        | (fst $ head x) /= S = ST.evalState (
            do
                a <- findBefore (Static, "{")
                b <- findPairM (Static, "{") (Static, "}")
                return $ concat [a, b]) x
        | otherwise = []

getLexem :: ([Token] -> [Token]) -> String -> [Token] -> (String, [Token])
getLexem f id tokens = (id, f tokens)

splitOnBaseLexems :: [Token] -> [(String, [Token])]
splitOnBaseLexems [] = []
splitOnBaseLexems tokens@(x:xs)
    | fst x == S = splitOnBaseLexems xs
    | otherwise =
        case curLexem tokens of
            Just l -> l : splitOnBaseLexems (snd $ splitAt (length $ snd l) tokens)
            Nothing -> []
            where curLexem t = find (\l -> snd l /= []) [
                    (getLexem (charset_lex) "charset" t),
                    (getLexem (import_lex) "import" t),
                    (getLexem (namespace_lex) "namespace" t),
                    (getLexem (page_lex) "page" tokens),
                    (getLexem (font_face_lex) "font-face" t),
                    (getLexem (media_lex) "media" t),
                    (getLexem (ruleset_lex) "ruleset" t)]
h :: Parser Char
h = hexDigit <?> "h"

nonascii :: Parser String
nonascii = do
        count 1 (satisfy (\x -> x >= '\o240' && x <= '\o4177777'))
        <?> "nonascii"

unicode :: Parser String
unicode = concat <$> sequence [
        string "\\",
        (try (count 6 h)
            <|> many1 h),
        (option "" $ do
            try (string "\r\n")
            <|> count 1 (oneOf "\t\r\n\f"))]
        <?> "unicode"

escape :: Parser String
escape = do
        try unicode
        <|> concat <$> sequence [string "\\",
                count 1 (noneOf $ "\r\n\f" ++
                                  ['0'..'9'] ++
                                  ['a'..'f'] ++
                                  ['A'..'F'])]
        <?> "escape"

nmstart :: Parser String
nmstart = do
        count 1 (oneOf $ "_" ++ ['a'..'z'] ++ ['A'..'Z'])
        <|> nonascii
        <|> escape
        <?> "nmstart"

nmchar :: Parser String
nmchar = do
        count 1 (oneOf $ concat [['a'..'z'], ['A'..'Z'], ['0'..'9'], "_-"])
        <|> nonascii
        <|> escape
        <?> "nmchar"

string1 :: Parser String
string1 = concat <$> sequence [
            string "\"",
            (many $ noneOf "\""),
            string "\""]
        <?> "string1"

string2 :: Parser String
string2 = concat <$> sequence [
            string "\'",
            (many $ noneOf "\'"),
            string "\'"]
        <?> "string2"

ident :: Parser String
ident = do
        minus <- option "" (string "-")
        first <- nmstart
        any <- many nmchar
        return $ concat [minus, first, concat any]
        <?> "ident"

name :: Parser String
name = concat <$> many1 nmchar
       <?> "name"

num :: Parser String
num = do
        try $ concat <$> sequence [
             many (oneOf ['0'..'9']),
             string ".",
             many1 (oneOf ['0'..'9'])]
        <|> many1 (oneOf ['0'..'9'])
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
        try (string "\r\n")
        <|> count 1 (oneOf "\n\r\f")
        <?> "nl"

range :: Parser String
range = do
        return ""
        <?> "range"

sym_d :: Parser String
sym_d = concat <$> count 1 (string "d")
        <|> do
            start <- string "\\"
            zero <- try (count 4 (string "0"))
                <|> many (string "0")
            n <- string "44"
                <|> string "64"
            end <- option "" $ do
                    try (string "\r\n")
                    <|> count 1 (oneOf " \t\r\n\f")
            return $ start ++ (concat zero) ++ n ++ end
        <?> "sym_d"

sym_e :: Parser String
sym_e = concat <$> count 1 (string "e")
        <|> do
            start <- string "\\"
            zero <- try (count 4 (string "0"))
                <|> many (string "0")
            n <- string "45"
                <|> string "65"
            end <- option "" $ do
                    try (string "\r\n")
                    <|> count 1 (oneOf " \t\r\n\f")
            return $ start ++ (concat zero) ++ n ++ end
        <?> "sym_e"

sym_n :: Parser String
sym_n = concat <$> count 1 (string "n")
        <|> try (do
            start <- string "\\"
            zero <- try (count 4 (string "0"))
                <|> many (string "0")
            n <- string "4e"
                <|> string "6e"
            end <- option "" $ do
                    try (string "\r\n")
                    <|> count 1 (oneOf " \t\r\n\f")
            return $ start ++ (concat zero) ++ n ++ end)
        <|> string "\\n"
        <?> "sym_n"

sym_o :: Parser String
sym_o = concat <$> count 1 (string "o")
        <|> try (do
            start <- string "\\"
            zero <- try (count 4 (string "0"))
                <|> many (string "0")
            n <- string "4f"
                <|> string "6f"
            end <- option "" $ do
                    try (string "\r\n")
                    <|> count 1 (oneOf " \t\r\n\f")
            return $ start ++ (concat zero) ++ n ++ end)
        <|> string "\\o"
        <?> "sym_o"

sym_t :: Parser String
sym_t = concat <$> count 1 (string "t")
        <|> try (do
            start <- string "\\"
            zero <- try (count 4 (string "0"))
                <|> many (string "0")
            n <- string "54"
                <|> string "74"
            end <- option "" $ do
                    try (string "\r\n")
                    <|> count 1 (oneOf " \t\r\n\f")
            return $ start ++ (concat zero) ++ n ++ end)
        <|> string "\\t"
        <?> "sym_t"

sym_v :: Parser String
sym_v = concat <$> count 1 (string "v")
        <|> try (do
            start <- string "\\"
            zero <- try (count 4 (string "0"))
                <|> many (string "0")
            n <- string "58"
                <|> string "78"
            end <- option "" $ do
                    try (string "\r\n")
                    <|> count 1 (oneOf " \t\r\n\f")
            return $ start ++ (concat zero) ++ n ++ end)
        <|> string "\\v"
        <?> "sym_v"

_S :: Parser Token
_S = do
        s <- many1 $ (oneOf " \t\r\n\f")
        return $ (S, s)

_CDO :: Parser Token
_CDO = do
        string "<!--"
        return $ (Cdo, "<!--")

_CDC :: Parser Token
_CDC = do
        string "-->"
        return $ (Cdc, "-->")

_INCLUDES :: Parser Token
_INCLUDES = do
        string "~="
        return $ (Includes, "~=")

_DASHMATCH :: Parser Token
_DASHMATCH = do
        string "|="
        return $ (DashMatch, "|=")

_PREFIXMATCH :: Parser Token
_PREFIXMATCH = do
        string "^="
        return $ (PrefixMatch, "^=")

_SUFFIXMATCH :: Parser Token
_SUFFIXMATCH = do
        string "$="
        return $ (SuffixMatch, "$=")

_SUBSTRINGMATCH :: Parser Token
_SUBSTRINGMATCH = do
        string "*="
        return $ (SubStringMatch, "*=")

_STRING ::Parser Token
_STRING = do
        s <- _string
        return $ (String', s)

_IDENT :: Parser Token
_IDENT = do
        i <- ident
        return $ (Ident, i)

_HASH :: Parser Token
_HASH = do
        string "#"
        hashname <- name
        return $ (Hash, (concat ["#", hashname]))

_PLUS :: Parser Token
_PLUS = do
        x <- w
        string "+"
        return $ (Plus, (x ++ "+"))

_GREATER :: Parser Token
_GREATER = do
        x <- w
        string ">"
        return $ (Greater, (x ++ ">"))

_COMMA :: Parser Token
_COMMA = do
        x <- w
        string ","
        return $ (Comma, (x ++ ","))

_TILDE :: Parser Token
_TILDE = do
        x <- w
        string "~"
        return $ (Tilde, (x ++ "~"))

_NOT :: Parser Token
_NOT = do
        res <- sequence [
            string ":",
            sym_n,
            sym_o,
            sym_t,
            string "("]
        return $ (Not, (concat res))

_ATKEYWORD :: Parser Token
_ATKEYWORD = do
        string "@"
        i <- ident
        return $ (AtKeyWord, ("@" ++ i))

_DIMENSION :: Parser Token
_DIMENSION = do
        n <- num
        i <- ident
        return $ (Dimension, (n ++ i))

_IMPORT_SYM :: Parser Token
_IMPORT_SYM = do
        string "@import"
        return $ (ImportSym, "@import")

_PAGE_SYM :: Parser Token
_PAGE_SYM = do
        string "@page"
        return $ (PageSym, "@page")

_MEDIA_SYM :: Parser Token
_MEDIA_SYM = do
        string "@media"
        return $ (MediaSym, "@media")

_FONT_FACE_SYM :: Parser Token
_FONT_FACE_SYM = do
        string "@font-face"
        return $ (FontFaceSym, "@font-face")

_CHARSET_SYM :: Parser Token
_CHARSET_SYM = do
        string "@charset"
        return $ (CharsetSym, "@charset")

_NAMESPACE_SYM :: Parser Token
_NAMESPACE_SYM = do
        string "@namespace"
        return $ (NamespaceSym, "@namespace")

_IMPORTANT_SYM :: Parser Token
_IMPORTANT_SYM = do
        char '!'
        space <- w
        string "important"
        return $ (ImportantSym, (concat ["!", space, "important"]))

_EMS :: Parser Token
_EMS = do
        n <- num
        string "em"
        return $ (Ems, (concat [n, "em"]))

_EXS :: Parser Token
_EXS = do
        n <- num
        string "ex"
        return $ (Exs, (concat [n, "em"]))

_LENGTH :: Parser Token
_LENGTH = do
        n <- num
        u <- string "px"
            <|> string "cm"
            <|> string "mm"
            <|> string "in"
            <|> string "pt"
            <|> string "pc"
        return $ (Length, (concat [n,u]))

_ANGLE :: Parser Token
_ANGLE = do
        n <- num
        u <- string "deg"
            <|> string "rad"
            <|> string "grad"
        return $ (Angle, (concat [n,u]))

_TIME :: Parser Token
_TIME = do
        n <- num
        u <- string "ms"
            <|> string "s"
        return $ (Time, (concat [n,u]))


_FREQ :: Parser Token
_FREQ = do
        n <- num
        u <- string "Hz"
            <|> string "kHz"
        return $ (Freq, (concat [n,u]))

_DIMEN :: Parser Token
_DIMEN = do
        n <- num
        i <- ident
        return $ (Dimen, (concat [n, i]))

_PERCENTAGE :: Parser Token
_PERCENTAGE = do
        n <- num
        char '%'
        return $ (Percentage, (concat [n, "%"]))

_NUMBER :: Parser Token
_NUMBER = do
        n <- num
        return $ (Number, n)

_URI :: Parser Token
_URI = do
        res <- sequence [
            string "url(",
            w,
            many $ noneOf " \t\r\n\f)",
            w,
            string ")"]
        return $ (Uri, (concat res))

_MEDIA_LIMIT :: Parser Token
_MEDIA_LIMIT = do
        res <- sequence [
            string "(",
            w,
            many $ noneOf "\t\r\n\f)",
            w,
            string ")"]
        return $ (MediaLimit, (concat res))

_FUNCTION :: Parser Token
_FUNCTION = do
        i <- ident
        char '('
        return $ (Function, (concat [i, "("]))

_UNICODERANGE :: Parser Token
_UNICODERANGE = do
        res <- sequence [
            string "U+",
            (do
                try (count 6 h)
                <|> many1 h),
            count 1 (char '-'),
            (do
                try (count 6 h)
                <|> many1 h)]
        return $ (UnicodeRange, (concat res))

_STATIC :: String -> Parser Token
_STATIC s = do
        str <- string s
        return $ (Static, str)
