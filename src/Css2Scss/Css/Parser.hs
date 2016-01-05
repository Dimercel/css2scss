module Css2Scss.Css.Parser
    ( stylesheet
    , property
    , operator
    , unary_operator
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

property :: Parser [L.Token]
property = ((:) <$> L._IDENT <*> many L._S)

operator :: Parser [L.Token]
operator = do
        (:) <$> L._STATIC "/" <*> many L._S
        <|> (:) <$> L._STATIC "," <*> many L._S
        <|> return []


unary_operator :: Parser [L.Token]
unary_operator = do
        s <- count 1 (oneOf "+-")
        return $ [L.Static s]

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
