{-# LANGUAGE QuasiQuotes #-}

module Css2Scss.Css.Parser
    ( stylesheet
    , _import
    , selectors_group
    , simple_selector_sequence
    , type_selector
    , namespace
    , namespace_prefix
    , media
    , medium
    , page
    , pseudo_page
    , font_face
    , operator
    , combinator
    , unary_operator
    , property
    , ruleset
    , selector
    , universal
    , _class
    , element_name
    , attrib
    , pseudo
    , declaration
    , prio
    , functional_pseudo
    , expression
    , negation
    , negation_arg
    , expr
    , term
    , function
    , hexcolor

    , preprocessor
    ) where


import Text.ParserCombinators.Parsec
import Text.Regex.PCRE.Heavy (gsub, re)
import Css2Scss.Css.Lexer as L


preprocessor :: String -> String
preprocessor text = do
        ignore_comments text

ignore_comments :: String -> String
ignore_comments css =
        gsub [re|\/\*[^*]*\*+([^/*][^*]*\*+)*\/|] (" " :: String) css


stylesheet :: Parser [L.Token]
stylesheet = concat <$> sequence [
        (option [] $ do
            res <- sequence [
                count 1 (L._CHARSET_SYM),
                many L._S,
                count 1 (L._STRING),
                many L._S,
                count 1 (L._STATIC ";")]
            return $ concat res),
        (many $ do
                L._S
                <|> L._CDO
                <|> L._CDC),
        (concat <$> (many $ do
            i <- _import
            s <- many $ do
                    L._S
                    <|> L._CDO
                    <|> L._CDC
            return $ i ++ s)),
        (concat <$> (many $ do
            i <- namespace
            s <- many $ do
                    L._S
                    <|> L._CDO
                    <|> L._CDC
            return $ i ++ s)),
        (concat <$> (many $ do
            res <- sequence [
                (ruleset
                <|> try media
                <|> try page
                <|> try font_face),
                (many $ do
                    L._S
                    <|> L._CDO
                    <|> L._CDC)]
            return $ concat res))]

_import :: Parser [L.Token]
_import = concat <$> sequence [
        count 1 L._IMPORT_SYM,
        many L._S,
        (count 1 L._STRING
        <|> count 1 L._URI),
        many L._S,
        (option [] $ do
            m <- medium
            res <- many $ sequence[
                count 1 (L._STATIC ","),
                many L._S,
                medium]
            return $ concat [m, concat $ concat res]),
        count 1 (L._STATIC ";"),
        many L._S]

selectors_group :: Parser [L.Token]
selectors_group = do
        s <- selector
        ms <- many $ do
                concat <$> sequence [
                    count 1 L._COMMA,
                    many L._S,
                    selector]
        return $ s ++ (concat ms)

simple_selector_sequence :: Parser [L.Token]
simple_selector_sequence = concat <$> sequence [
        (option [] (try universal
            <|> type_selector)),
        (concat <$> (many $ do
            count 1 L._HASH
            <|> _class
            <|> try negation
            <|> pseudo
            <|> attrib))]


type_selector :: Parser [L.Token]
type_selector = do
        try $ do
            e <- element_name
            notFollowedBy $ char '|'
            return $ e
        <|> do
                n <- option [] namespace_prefix
                e <- element_name
                return $ n ++ e

namespace :: Parser [L.Token]
namespace = concat <$> sequence [
        count 1 L._NAMESPACE_SYM,
        many L._S,
        (do
            try $ count 1 L._URI
            <|> do
                    concat <$> sequence [
                        (option [] $ do
                            concat <$> sequence [
                                namespace_prefix,
                                many1 L._S]),
                        (count 1 $ do
                            L._URI
                            <|> L._STRING)]),
        many L._S,
        count 1 (L._STATIC ";"),
        many L._S]

namespace_prefix :: Parser [L.Token]
namespace_prefix = concat <$> sequence [
        (option [] (count 1 (L._STATIC "*")
            <|> count 1 L._IDENT)),
        option [] (count 1 (L._STATIC "|"))]

media :: Parser [L.Token]
media = concat <$> sequence [
            count 1 L._MEDIA_SYM,
            many L._S,
            medium,
            concat <$> (many $ do
                res <- sequence [
                    (count 1 (L._STATIC ",")
                    <|> count 1 (L._STATIC "and")),
                    many L._S,
                    medium]
                return $ concat res),
            count 1 (L._STATIC "{"),
            many L._S,
            concat <$> many ruleset,
            count 1 (L._STATIC "}"),
            many L._S]

medium :: Parser [L.Token]
medium = concat <$> sequence [
        (count 1 L._MEDIA_LIMIT
        <|> count 1 L._IDENT),
        many L._S]

page :: Parser [L.Token]
page = concat <$> sequence [
        count 1 L._PAGE_SYM,
        many L._S,
        option [] (count 1 L._IDENT),
        option [] pseudo_page,
        many L._S,
        count 1 (L._STATIC "{"),
        many L._S,
        declaration,
        concat <$> (many $ do
            res <- sequence [
                count 1 (L._STATIC ";"),
                many L._S,
                declaration]
            return $ concat res),
        count 1 (L._STATIC "}"),
        many L._S]

pseudo_page :: Parser [L.Token]
pseudo_page = (:) <$> L._STATIC ":" <*> count 1 L._IDENT

font_face :: Parser [L.Token]
font_face = concat <$> sequence [
        count 1 L._FONT_FACE_SYM,
        many L._S,
        count 1 (L._STATIC "{"),
        many L._S,
        declaration,
        concat <$> (many $ do
            res <- sequence [
                count 1 (L._STATIC ";"),
                many L._S,
                declaration]
            return $ concat res),
        count 1 (L._STATIC "}"),
        many L._S]

operator :: Parser [L.Token]
operator = do
        (:) <$> L._STATIC "/" <*> many L._S
        <|> (:) <$> L._STATIC "," <*> many L._S
        <|> return []

combinator :: Parser [L.Token]
combinator = do
        do
            c <- try L._GREATER
            s <- many L._S
            return $ c : s
        <|> do
            c <- try L._PLUS
            s <- many L._S
            return $ c : s
        <|> do
            c <- try L._TILDE
            s <- many L._S
            return $ c : s
        <|> many1 L._S

unary_operator :: Parser [L.Token]
unary_operator = do
        s <- count 1 (oneOf "+-")
        return $ [(L.Static, s)]

property :: Parser [L.Token]
property = ((:) <$> L._IDENT <*> many L._S)

ruleset :: Parser [L.Token]
ruleset = concat <$> sequence [
            selectors_group,
            count 1 (L._STATIC "{"),
            many L._S,
            declaration,
            concat <$> (many $ do
                concat <$> sequence [
                    count 1 (L._STATIC ";"),
                    many L._S,
                    declaration]),
            count 1 (L._STATIC "}"),
            many L._S]

selector :: Parser [L.Token]
selector = concat <$> sequence [
        simple_selector_sequence,
        (concat <$> (many $ do
            c <- combinator
            s <- simple_selector_sequence
            return $ c ++ s))]

universal :: Parser [L.Token]
universal = do
        try $ do
            s <- count 1 (L._STATIC "*")
            notFollowedBy $ char '|'
            return s
        <|> do
                n <- option [] namespace_prefix
                s <- count 1 (L._STATIC "*")
                return $ n ++ s

_class :: Parser [L.Token]
_class = ((:) <$> L._STATIC "." <*> count 1 L._IDENT)

element_name :: Parser [L.Token]
element_name = count 1 L._IDENT

attrib :: Parser [L.Token]
attrib = concat <$> sequence [
        count 1 (L._STATIC "["),
        many L._S,
        option [] namespace_prefix,
        many L._S,
        (option [] $ concat <$> sequence [
            (count 1 L._PREFIXMATCH
            <|> count 1 L._SUFFIXMATCH
            <|> count 1 L._SUBSTRINGMATCH
            <|> count 1 L._INCLUDES
            <|> count 1 L._DASHMATCH
            <|> count 1 (L._STATIC "=")),
            many L._S,
            (count 1 L._IDENT
            <|> count 1 L._STRING),
            many L._S]),
        count 1 (L._STATIC "]")]

pseudo :: Parser [L.Token]
pseudo = concat <$> sequence [
            (count 1 (L._STATIC ":")),
            option [] (count 1 (L._STATIC ":")),
            (try functional_pseudo
             <|> count 1 L._IDENT)]


declaration :: Parser [L.Token]
declaration = do
        do
            res <- sequence [property,
                             count 1 (L._STATIC ":"),
                             (many L._S),
                             expr]
            pr <- option [] prio
            return $ (concat res) ++ pr
        <|> return []

prio :: Parser [L.Token]
prio =  ((:) <$> L._IMPORTANT_SYM <*> many L._S)



functional_pseudo :: Parser [L.Token]
functional_pseudo = concat <$> sequence [
        count 1 L._FUNCTION,
        many L._S,
        expression,
        count 1 (L._STATIC ")")]

expression :: Parser [L.Token]
expression = concat <$> (many1 $ do
            concat <$> sequence [
                (count 1 L._PLUS
                <|> count 1 (L._STATIC "-")
                <|> count 1 L._DIMENSION
                <|> count 1 L._NUMBER
                <|> count 1 L._IDENT
                <|> count 1 L._STRING),
                many L._S])

negation :: Parser [L.Token]
negation = concat <$> sequence [
        count 1 L._NOT,
        many L._S,
        negation_arg,
        many L._S,
        count 1 (L._STATIC ")")]

negation_arg :: Parser [L.Token]
negation_arg = do
        try type_selector
        <|> universal
        <|> count 1 L._HASH
        <|> _class
        <|> pseudo
        <|> attrib

expr :: Parser [L.Token]
expr = do
        t <- term
        o <- many $ do
            concat <$> sequence [operator, term]
        return $ t ++ (concat o)

term :: Parser [L.Token]
term = concat <$> sequence [
        (option [] unary_operator),
        (do
            try hexcolor
            <|> try ((:) <$> L._UNICODERANGE <*> many L._S)
            <|> try ((:) <$> L._URI <*> many L._S)
            <|> do
                    try function
                    <|> try ((:) <$> L._PERCENTAGE <*> many L._S)
                    <|> try ((:) <$> L._LENGTH <*> many L._S)
                    <|> try ((:) <$> L._EMS <*> many L._S)
                    <|> try ((:) <$> L._EXS <*> many L._S)
                    <|> try ((:) <$> L._ANGLE <*> many L._S)
                    <|> try ((:) <$> L._TIME <*> many L._S)
                    <|> try ((:) <$> L._FREQ <*> many L._S)
                    <|> try ((:) <$> L._NUMBER <*> many L._S)
            <|> try ((:) <$> L._IDENT <*> many L._S)
            <|> (:) <$> L._STRING <*> many L._S)]

function :: Parser [L.Token]
function = concat <$> sequence [count 1 L._FUNCTION, (many L._S), expr,
                                count 1 (L._STATIC ")"), (many L._S)]

hexcolor :: Parser [L.Token]
hexcolor = concat <$> sequence [count 1 L._HASH, (many L._S)]
