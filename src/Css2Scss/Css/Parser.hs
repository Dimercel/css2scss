module Css2Scss.Css.Parser 
    ( stylesheet
    , property
    , term
    , function
    , hexcolor
    ) where


import Text.ParserCombinators.Parsec
import Css2Scss.Css.Lexer as L


stylesheet :: String -> String
stylesheet _ = "It is work!"

property :: Parser [L.Token]
property = do
        i <- L._IDENT
        s <- many L._S
        return $ i:s

term :: Parser [L.Token]
term = do
        uo <- option [] unary_operator
        n <- do
            L._NUMBER
            <|> L._PERCENTAGE
            <|> L._LENGTH
            <|> L._EMS
            <|> L._EXS
            <|> L._ANGLE
            <|> L._TIME
            <|> L._FREQ
            <|> function
            many L._S
        return $ n

function :: Parser [L.Token]
function = do
        f <- L._FUNCTION
        s1 <- L._S
        e <- expr
        char ')'
        s2 <- L._S
        return $ [f, s1] ++ e ++ [s2]

hexcolor :: Parser [L.Token]
hexcolor = do
        h <- L._HASH
        s <- many L._S
        return $ h:s
