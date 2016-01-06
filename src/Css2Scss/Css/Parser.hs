module Css2Scss.Css.Parser
    ( stylesheet
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
    , _class
    , element_name
    , attrib
    , pseudo
    , declaration
    , prio
    , expr
    , term
    , function
    , hexcolor
    ) where


import Text.ParserCombinators.Parsec
import Css2Scss.Css.Lexer as L


stylesheet :: String -> String
stylesheet _ = "It is work!"

namespace_prefix :: Parser [L.Token]
namespace_prefix = count 1 (L._IDENT)

media :: Parser [L.Token]
media = do
        res <- sequence [
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
        return $ concat res

medium :: Parser [L.Token]
medium = (:) <$> L._IDENT <*> many L._S

page :: Parser [L.Token]
page = do
        res <- sequence [
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
        return $ concat res

pseudo_page :: Parser [L.Token]
pseudo_page = (:) <$> L._STATIC ":" <*> count 1 L._IDENT

font_face :: Parser [L.Token]
font_face = do
        res <- sequence [
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
        return $ concat res

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
ruleset = do
        res <- sequence [
            selector,
            concat <$> (many $ do
                res <- sequence [
                    count 1 (L._STATIC ","),
                    many L._S,
                    selector]
                return $ concat res),
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
        return $ concat res

selector :: Parser [L.Token]
selector = do
        res <- sequence [
            simple_selector,
            (option [] $ do
                combinator
                simple_selector)]
        return $ concat res

simple_selector :: Parser [L.Token]
simple_selector = do
        res <- sequence [
            option [] element_name,
            (option [] $ do
                count 1 L._HASH
                <|> _class
                <|> attrib
                <|> pseudo),
            many L._S]
        return $ concat res

_class :: Parser [L.Token]
_class = ((:) <$> L._STATIC "." <*> count 1 L._IDENT)

element_name :: Parser [L.Token]
element_name = count 1 L._IDENT <|> count 1 (L._STATIC "*")

attrib :: Parser [L.Token]
attrib = do
        name <- sequence [count 1 (L._STATIC "["), many L._S,
                          count 1 (L._IDENT), many L._S]
        x <- option [] $ do
           s1 <- do
               L._STATIC "="
               <|> L._INCLUDES
               <|> L._DASHMATCH
           sp1 <- many L._S
           s2 <- do
              L._IDENT
              <|> L._STRING
           sp2 <- many L._S
           return $ concat [[s1], sp1, [s2], sp2]
        end <- count 1 (L._STATIC "]")
        return $ concat [concat name, x, end]

pseudo :: Parser [L.Token]
pseudo = do
        start <- L._STATIC ":"
        i <- do count 1 L._IDENT
                <|> do
                       res <- sequence [count 1 L._FUNCTION, many L._S,
                                        count 1 L._IDENT, many L._S,
                                        count 1 (L._STATIC ")")]
                       return $ concat res
        return $ start : i

declaration :: Parser [L.Token]
declaration = do
         do
            res <- sequence [property, count 1 (L._STATIC ":"), (many L._S), expr]
            pr <- option [] prio
            return $ (concat res) ++ pr
        <|> return []

prio :: Parser [L.Token]
prio =  ((:) <$> L._IMPORTANT_SYM <*> many L._S)

expr :: Parser [L.Token]
expr = do
        t <- term
        o <- many $ do
            res <- sequence [operator, term]
            return $ concat res
        return $ t ++ (concat o)

term :: Parser [L.Token]
term = do
        uo <- option [] unary_operator
        x <- do
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
            <|> (:) <$> L._STRING <*> many L._S
        return $ uo ++ x

function :: Parser [L.Token]
function = do
        res <- sequence [count 1 L._FUNCTION, (many L._S), expr,
                         count 1 (L._STATIC ")"), (many L._S)]
        return $ concat res

hexcolor :: Parser [L.Token]
hexcolor = do
        res <- sequence [count 1 L._HASH, (many L._S)]
        return $ concat res
