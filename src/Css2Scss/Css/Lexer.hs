module Css2Scss.Css.Lexer
    ( Token (..)
    , TokenId(..)
    , splitOnBaseLexems
    , tokensData
    , chomp
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


tokensData :: [Token] -> String
tokensData = concatMap snd

-- | Обрезает токены пробельных символов в начале
-- и конце списка
chomp :: [Token] -> [Token]
chomp x = dropWhileEnd (isSpaceToken) (dropWhile (isSpaceToken) x)

isSpaceToken :: Token -> Bool
isSpaceToken t = fst t == S

isSpaceLexem :: [Token] -> Bool
isSpaceLexem [] = False
isSpaceLexem t = all (isSpaceToken) t

isCharsetLexem :: [Token] -> Bool
isCharsetLexem [] = False
isCharsetLexem (x:xs) = fst x == CharsetSym

isImportLexem :: [Token] -> Bool
isImportLexem [] = False
isImportLexem (x:xs) = fst x == ImportSym

isNamespaceLexem :: [Token] -> Bool
isNamespaceLexem [] = False
isNamespaceLexem (x:xs) = fst x == NamespaceSym

isPageLexem :: [Token] -> Bool
isPageLexem [] = False
isPageLexem (x:xs) = fst x == PageSym

isFontFaceLexem :: [Token] -> Bool
isFontFaceLexem [] = False
isFontFaceLexem (x:xs) = fst x == FontFaceSym

isMediaLexem :: [Token] -> Bool
isMediaLexem [] = False
isMediaLexem (x:xs) = fst x == MediaSym

isRulesetLexem :: [Token] -> Bool
isRulesetLexem [] = False
isRulesetLexem t
        | isSpaceLexem t = False
        | otherwise = True

splitOnBaseLexems :: [[Token]] -> [(String, [Token])]
splitOnBaseLexems tokens = notError $ map (buildLexem) (filter (/= []) tokens)
        where buildLexem x = case lexems x of
                                 Just l -> (snd l, chomp x)
                                 Nothing -> ("error", x)
              notError = filter (\x -> fst x /= "error")
              lexems l = find fst [
                    (isCharsetLexem l,   "charset"  ),
                    (isImportLexem l,    "import"   ),
                    (isNamespaceLexem l, "namespace"),
                    (isPageLexem l,      "page"     ),
                    (isFontFaceLexem l,  "font-face"),
                    (isMediaLexem l,     "media"    ),
                    (isRulesetLexem l,   "ruleset"  )]

h :: Parser Char
h = hexDigit <?> "h"

nonascii :: Parser String
nonascii =
  count 1 (satisfy (\x -> x >= '\o240' && x <= '\o4177777'))
  <?> "nonascii"

unicode :: Parser String
unicode = concat <$> sequence [
            string "\\",
            try (count 6 h)
                <|> many1 h,
            option "" $
                try (string "\r\n")
                <|> count 1 (oneOf "\t\r\n\f")]
            <?> "unicode"

escape :: Parser String
escape =
  try unicode
  <|> concat <$> sequence [
        string "\\",
        count 1 (noneOf $ "\r\n\f" ++
                          ['0'..'9'] ++
                          ['a'..'f'] ++
                          ['A'..'F'])]
  <?> "escape"

nmstart :: Parser String
nmstart =
  count 1 (oneOf $ "_" ++ ['a'..'z'] ++ ['A'..'Z'])
  <|> nonascii
  <|> escape
  <?> "nmstart"

nmchar :: Parser String
nmchar =
  count 1 (oneOf $ concat [['a'..'z'], ['A'..'Z'], ['0'..'9'], "_-"])
  <|> nonascii
  <|> escape
  <?> "nmchar"

string1 :: Parser String
string1 = concat <$> sequence [
            string "\"",
            many $ noneOf "\"",
            string "\""]
          <?> "string1"

string2 :: Parser String
string2 = concat <$> sequence [
            string "\'",
            many $ noneOf "\'",
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
_string = string1
          <|> string2
          <?> "_string"

url :: Parser String
url = do
        content <- many $
          count 1 (oneOf "!#$%&*-~")
          <|> nonascii
          <|> escape
        return $ concat content
        <?> "url"

w :: Parser String
w = many (oneOf " \t\r\n\f")
    <?> "w"

nl :: Parser String
nl = try (string "\r\n")
     <|> count 1 (oneOf "\n\r\f")
     <?> "nl"

range :: Parser String
range = return ""
        <?> "range"

sym_d :: Parser String
sym_d = concat <$>
  count 1 (string "d")
  <|> do
        start <- string "\\"
        zero <- try (count 4 (string "0"))
            <|> many (string "0")
        n <- string "44"
            <|> string "64"
        end <- option "" $
                try (string "\r\n")
                <|> count 1 (oneOf " \t\r\n\f")
        return $ start ++ concat zero ++ n ++ end
  <?> "sym_d"

sym_e :: Parser String
sym_e = concat <$>
  count 1 (string "e")
  <|> do
        start <- string "\\"
        zero <- try (count 4 (string "0"))
            <|> many (string "0")
        n <- string "45"
            <|> string "65"
        end <- option "" $
                try (string "\r\n")
                <|> count 1 (oneOf " \t\r\n\f")
        return $ start ++ concat zero ++ n ++ end
  <?> "sym_e"

sym_n :: Parser String
sym_n = concat <$>
  count 1 (string "n")
  <|> try
    (do
        start <- string "\\"
        zero <- try (count 4 (string "0"))
                <|> many (string "0")
        n <- string "4e"
            <|> string "6e"
        end <- option "" $
                  try (string "\r\n")
                  <|> count 1 (oneOf " \t\r\n\f")
        return $ start ++ concat zero ++ n ++ end)
  <|> string "\\n"
  <?> "sym_n"

sym_o :: Parser String
sym_o = concat <$>
  count 1 (string "o")
  <|> try (do
              start <- string "\\"
              zero <- try (count 4 (string "0"))
                  <|> many (string "0")
              n <- string "4f"
                  <|> string "6f"
              end <- option "" $
                      try (string "\r\n")
                      <|> count 1 (oneOf " \t\r\n\f")
              return $ start ++ concat zero ++ n ++ end)
  <|> string "\\o"
  <?> "sym_o"

sym_t :: Parser String
sym_t = concat <$>
  count 1 (string "t")
  <|> try (do
              start <- string "\\"
              zero <- try (count 4 (string "0"))
                      <|> many (string "0")
              n <- string "54"
                   <|> string "74"
              end <- option "" $
                       try (string "\r\n")
                       <|> count 1 (oneOf " \t\r\n\f")
              return $ start ++ concat zero ++ n ++ end)
  <|> string "\\t"
  <?> "sym_t"

sym_v :: Parser String
sym_v = concat <$>
  count 1 (string "v")
  <|> try (do
              start <- string "\\"
              zero <- try (count 4 (string "0"))
                      <|> many (string "0")
              n <- string "58"
                   <|> string "78"
              end <- option "" $
                       try (string "\r\n")
                       <|> count 1 (oneOf " \t\r\n\f")
              return $ start ++ concat zero ++ n ++ end)
  <|> string "\\v"
  <?> "sym_v"

_S :: Parser Token
_S = do
        s <- many1 (oneOf " \t\r\n\f")
        return (S, s)

_CDO :: Parser Token
_CDO = do
        string "<!--"
        return (Cdo, "<!--")

_CDC :: Parser Token
_CDC = do
        string "-->"
        return (Cdc, "-->")

_INCLUDES :: Parser Token
_INCLUDES = do
        string "~="
        return $ (Includes, "~=")

_DASHMATCH :: Parser Token
_DASHMATCH = do
        string "|="
        return (DashMatch, "|=")

_PREFIXMATCH :: Parser Token
_PREFIXMATCH = do
        string "^="
        return (PrefixMatch, "^=")

_SUFFIXMATCH :: Parser Token
_SUFFIXMATCH = do
        string "$="
        return (SuffixMatch, "$=")

_SUBSTRINGMATCH :: Parser Token
_SUBSTRINGMATCH = do
        string "*="
        return (SubStringMatch, "*=")

_STRING ::Parser Token
_STRING = do
        s <- _string
        return (String', s)

_IDENT :: Parser Token
_IDENT = do
        i <- ident
        return (Ident, i)

_HASH :: Parser Token
_HASH = do
        string "#"
        hashname <- name
        return (Hash, "#" ++ hashname)

_PLUS :: Parser Token
_PLUS = do
        x <- w
        string "+"
        return (Plus, x ++ "+")

_GREATER :: Parser Token
_GREATER = do
        x <- w
        string ">"
        return (Greater, x ++ ">")

_COMMA :: Parser Token
_COMMA = do
        x <- w
        string ","
        return (Comma, x ++ ",")

_TILDE :: Parser Token
_TILDE = do
        x <- w
        string "~"
        return (Tilde, x ++ "~")

_NOT :: Parser Token
_NOT = do
        res <- sequence [
            string ":",
            sym_n,
            sym_o,
            sym_t,
            string "("]
        return (Not, concat res)

_ATKEYWORD :: Parser Token
_ATKEYWORD = do
        string "@"
        i <- ident
        return (AtKeyWord, "@" ++ i)

_DIMENSION :: Parser Token
_DIMENSION = do
        n <- num
        i <- ident
        return (Dimension, n ++ i)

_IMPORT_SYM :: Parser Token
_IMPORT_SYM = do
        string "@import"
        return (ImportSym, "@import")

_PAGE_SYM :: Parser Token
_PAGE_SYM = do
        string "@page"
        return (PageSym, "@page")

_MEDIA_SYM :: Parser Token
_MEDIA_SYM = do
        string "@media"
        return (MediaSym, "@media")

_FONT_FACE_SYM :: Parser Token
_FONT_FACE_SYM = do
        string "@font-face"
        return (FontFaceSym, "@font-face")

_CHARSET_SYM :: Parser Token
_CHARSET_SYM = do
        string "@charset"
        return (CharsetSym, "@charset")

_NAMESPACE_SYM :: Parser Token
_NAMESPACE_SYM = do
        string "@namespace"
        return (NamespaceSym, "@namespace")

_IMPORTANT_SYM :: Parser Token
_IMPORTANT_SYM = do
        char '!'
        space <- w
        string "important"
        return (ImportantSym, concat ["!", space, "important"])

_EMS :: Parser Token
_EMS = do
        n <- num
        string "em"
        return (Ems, n ++ "em")

_EXS :: Parser Token
_EXS = do
        n <- num
        string "ex"
        return (Exs, n ++ "em")

_LENGTH :: Parser Token
_LENGTH = do
        n <- num
        u <- string "px"
            <|> string "cm"
            <|> string "mm"
            <|> string "in"
            <|> string "pt"
            <|> string "pc"
        return (Length, n ++ u)

_ANGLE :: Parser Token
_ANGLE = do
        n <- num
        u <- string "deg"
            <|> string "rad"
            <|> string "grad"
        return (Angle, n ++ u)

_TIME :: Parser Token
_TIME = do
        n <- num
        u <- string "ms"
            <|> string "s"
        return (Time, n ++ u)


_FREQ :: Parser Token
_FREQ = do
        n <- num
        u <- string "Hz"
             <|> string "kHz"
        return (Freq, n ++ u)

_DIMEN :: Parser Token
_DIMEN = do
        n <- num
        i <- ident
        return (Dimen, n ++ i)

_PERCENTAGE :: Parser Token
_PERCENTAGE = do
        n <- num
        char '%'
        return (Percentage, n ++ "%")

_NUMBER :: Parser Token
_NUMBER = do
        n <- num
        return (Number, n)

_URI :: Parser Token
_URI = do
        res <- sequence [
            string "url(",
            w,
            many $ noneOf " \t\r\n\f)",
            w,
            string ")"]
        return (Uri, concat res)

_MEDIA_LIMIT :: Parser Token
_MEDIA_LIMIT = do
        res <- sequence [
            string "(",
            w,
            many $ noneOf "\t\r\n\f)",
            w,
            string ")"]
        return (MediaLimit, concat res)

_FUNCTION :: Parser Token
_FUNCTION = do
        i <- ident
        char '('
        return (Function, i ++ "(")

_UNICODERANGE :: Parser Token
_UNICODERANGE = do
        res <- sequence [
            string "U+",
            try (count 6 h)
            <|> many1 h,
            count 1 (char '-'),
            try (count 6 h)
            <|> many1 h]
        return (UnicodeRange, concat res)

_STATIC :: String -> Parser Token
_STATIC s = do
              str <- string s
              return (Static, str)
