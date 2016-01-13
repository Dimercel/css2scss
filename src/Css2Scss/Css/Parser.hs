module Css2Scss.Css.Parser
    ( stylesheet
    , _import
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
    , simple_selector
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
    ) where


import Text.ParserCombinators.Parsec
import Css2Scss.Css.Lexer as L


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
                <|> media
                <|> page
                <|> font_face),
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

type_selector :: Parser [L.Token]
type_selector = concat <$> sequence [
        option [] namespace_prefix,
        try element_name]

namespace :: Parser [L.Token]
namespace = concat <$> sequence [
        count 1 L._NAMESPACE_SYM,
        many L._S,
        (lookAhead $ do
            namespace_prefix
            many1 L._S),
        (count 1 $ do
            L._URI
            <|> L._STRING),
        many L._S,
        count 1 (L._STATIC ";"),
        many L._S]

namespace_prefix :: Parser [L.Token]
namespace_prefix = concat <$> sequence [
        (option [] (do
            count 1 L._IDENT
            <|> count 1 (L._STATIC "*"))),
        (option [] (count 1 $ L._STATIC "|"))]

media :: Parser [L.Token]
media = concat <$> sequence [
            count 1 L._MEDIA_SYM,
            many L._S,
            option [] medium,
            concat <$> (many $ do
                res <- sequence [
                    count 1 (L._STATIC ","),
                    many L._S,
                    medium]
                return $ concat res),
            count 1 (L._STATIC "{"),
            many L._S,
            concat <$> many ruleset,
            count 1 (L._STATIC "}"),
            many L._S]

medium :: Parser [L.Token]
medium = (:) <$> L._IDENT <*> many L._S

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
        (:) <$> L._STATIC "+" <*> many L._S
        <|> (:) <$> L._STATIC ">" <*> many L._S
        <|> return []

unary_operator :: Parser [L.Token]
unary_operator = do
        s <- count 1 (oneOf "+-")
        return $ [L.Static s]

property :: Parser [L.Token]
property = ((:) <$> L._IDENT <*> many L._S)

ruleset :: Parser [L.Token]
ruleset = concat <$> sequence [
            selector,
            concat <$> (many $ do
                concat <$> sequence [
                    count 1 (L._STATIC ","),
                    many L._S,
                    selector]),
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
        simple_selector,
        (option [] $ do
             combinator
             simple_selector)]

simple_selector :: Parser [L.Token]
simple_selector =
        concat <$> sequence [
            option [] element_name,
            (option [] $ do
                count 1 L._HASH
                <|> _class
                <|> attrib
                <|> pseudo),
            many L._S]

universal :: Parser [L.Token]
universal = concat <$> sequence [
        option [] namespace_prefix,
        count 1 (L._STATIC "*")]

_class :: Parser [L.Token]
_class = ((:) <$> L._STATIC "." <*> count 1 L._IDENT)

element_name :: Parser [L.Token]
element_name = count 1 L._IDENT

attrib :: Parser [L.Token]
attrib = do
        name <- sequence [count 1 (L._STATIC "["), many L._S,
                          count 1 (L._IDENT),      many L._S]
        con <- option [] $ do
            concat <$> sequence [
                (count 1 (do
                    L._STATIC "="
                    <|> L._INCLUDES
                    <|> L._DASHMATCH)),
                many L._S,
                (count 1 (do
                   L._IDENT
                   <|> L._STRING)),
                many L._S]
        end <- count 1 (L._STATIC "]")
        return $ concat [concat name, con, end]

pseudo :: Parser [L.Token]
pseudo = concat <$> sequence [
            (count 1 (L._STATIC ":")),
            option [] (count 1 (L._STATIC ":")),
            (do
                try (count 1 L._IDENT)
                <|> functional_pseudo)]


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
            <|> try ((:) <$> L._IDENT <*> many L._S)
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
            <|> (:) <$> L._STRING <*> many L._S)]

function :: Parser [L.Token]
function = concat <$> sequence [count 1 L._FUNCTION, (many L._S), expr,
                                count 1 (L._STATIC ")"), (many L._S)]

hexcolor :: Parser [L.Token]
hexcolor = concat <$> sequence [count 1 L._HASH, (many L._S)]
