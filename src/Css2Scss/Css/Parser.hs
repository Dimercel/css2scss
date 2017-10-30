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
import Data.String.Utils (strip)

import Css2Scss.Css.Lexer as L
import Css2Scss.Css ( Rule(..)
                    , Media(..)
                    , Definition(..)
                    , DefinitionT(..)
                    , CssItem (..)
                    , SelectorT
                    , CompSelector
                    , makeRule)


-- Вырезает комментарии из входного потока.
preprocessor :: String -> String
preprocessor = ignoreComments

ignoreComments :: String -> String
ignoreComments =
        gsub [re|\/\*[^*]*\*+([^/*][^*]*\*+)*\/|] (" " :: String)

decl2String :: (String, String) -> String
decl2String (x, y) = x ++ y


stylesheet :: Parser [CssItem]
stylesheet = do
  ch <- option [] $ do
      try L._CHARSET_SYM
      many L._S
      s <- L._STRING
      many L._S
      L._STATIC ";"
      return [s, (Static, ";")]

  many $ L._S
         <|> L._CDO
         <|> L._CDC

  imp <- many $ do
           i <- try _import
           many $ L._S
                  <|> L._CDO
                  <|> L._CDC
           return i

  ns <- many $ do
          n <- try namespace
          many $ L._S
                <|> L._CDO
                <|> L._CDC
          return n

  css <- many $ do
    x <- (RuleItem <$> ruleset)
         <|> try (MediaItem <$> media)
         <|> try (DefItem <$> page)
         <|> try (DefItem <$> font_face)
    many $ L._S
           <|> L._CDO
           <|> L._CDC
    return x

  return $ concat [
      if null ch then [] else [DefItem (Definition Charset (tokensData ch))],
      if null imp then [] else map DefItem imp,
      if null ns then [] else map DefItem ns,
      css]

_import :: Parser Definition
_import =
  do
    count 1 L._IMPORT_SYM
    many L._S
    res <-
      concat <$> sequence [
        count 1 L._STRING
        <|> count 1 L._URI,
        many L._S,
        option [] $ do
          m <- medium
          res <- many $ sequence[
            count 1 (L._STATIC ","),
            many L._S,
            medium]
          return $ m ++ concat (concat res),
        count 1 (L._STATIC ";"),
        many L._S]
    return $ Definition Import (strip $ tokensData res)

selectors_group :: Parser CompSelector
selectors_group = do
        s <- selector
        ms <- many $ do
                count 1 L._COMMA
                many L._S
                selector
        return $ s : ms

simple_selector_sequence :: Parser String
simple_selector_sequence =
  tokensData <$> concat <$> sequence [
    option [] (try universal
              <|> type_selector),
    concat <$> many (count 1 L._HASH
                     <|> _class
                     <|> try negation
                     <|> pseudo
                     <|> attrib)]

type_selector :: Parser [L.Token]
type_selector =
  try $ do
      e <- element_name
      notFollowedBy $ char '|'
      return e
  <|> do
          n <- option [] namespace_prefix
          e <- element_name
          return $ n ++ e

namespace :: Parser Definition
namespace =
  do
    count 1 L._NAMESPACE_SYM
    many L._S
    res <-
      concat <$> sequence [
        try $ count 1 L._URI
        <|> concat <$> sequence [
              option [] $
                concat <$> sequence [
                    namespace_prefix,
                    many1 L._S],
              count 1 $
                L._URI
                <|> L._STRING],
        many L._S,
        count 1 (L._STATIC ";"),
        many L._S]
    return $ Definition Namespace (strip $ tokensData res)

namespace_prefix :: Parser [L.Token]
namespace_prefix =
  concat <$> sequence [
    option [] (count 1 (L._STATIC "*")
               <|> count 1 L._IDENT),
    option [] (count 1 (L._STATIC "|"))]

media :: Parser Media
media = do
  count 1 L._MEDIA_SYM
  many L._S
  m <- medium
  s <- concat <$> many (do
      res <- sequence [
          count 1 L._COMMA
          <|> count 1 (L._STATIC "and"),
          many L._S,
          medium]
      return $ concat res)
  count 1 (L._STATIC "{")
  many L._S
  rules <- many ruleset
  count 1 (L._STATIC "}")
  many L._S
  return $ Media (tokensData m ++ tokensData s) rules

medium :: Parser [L.Token]
medium = concat <$> sequence [
        count 1 L._MEDIA_LIMIT
        <|> count 1 L._IDENT,
        many L._S]

page :: Parser Definition
page =
  do
    count 1 L._PAGE_SYM
    many L._S
    res <-
      concat <$> sequence [
        option [] (count 1 L._IDENT),
        option [] pseudo_page,
        many L._S,
        count 1 (L._STATIC "{"),
        many L._S,
        (do
           d <- declaration
           return [(L.Static, decl2String d)]),
        concat <$> many (do
            res <- sequence [
                count 1 (L._STATIC ";"),
                many L._S,
                (do
                    d <- declaration
                    return [(L.Static, decl2String d)])]
            return $ concat res),
        count 1 (L._STATIC "}"),
        many L._S]
    return $ Definition Page (strip $ tokensData res)

pseudo_page :: Parser [L.Token]
pseudo_page = (:) <$> L._STATIC ":" <*> count 1 L._IDENT

font_face :: Parser Definition
font_face =
  do
    count 1 L._FONT_FACE_SYM
    many L._S
    res <-
      concat <$> sequence [
          count 1 (L._STATIC "{"),
          many L._S,
          (do
            d <- declaration
            return [(L.Static, decl2String d)]),
          concat <$> many (do
              res <- sequence [
                  count 1 (L._STATIC ";"),
                  many L._S,
                  (do
                    d <- declaration
                    return [(L.Static, decl2String d)])]
              return $ concat res),
          count 1 (L._STATIC "}"),
          many L._S]
    return $ Definition FontFace (strip $ tokensData res)

operator :: Parser [L.Token]
operator =
  (:) <$> L._STATIC "/" <*> many L._S
  <|> (:) <$> L._STATIC "," <*> many L._S
  <|> return []

combinator :: Parser [L.Token]
combinator =
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
        return [(L.Static, s)]

property :: Parser [L.Token]
property = (:) <$> L._IDENT <*> many L._S

ruleset :: Parser Rule
ruleset = do
  g <- selectors_group
  count 1 (L._STATIC "{")
  many L._S
  d <- declaration
  ds <- many $
    do
      count 1 (L._STATIC ";")
      many L._S
      d <- declaration
      return d
  count 1 (L._STATIC "}")
  many L._S
  return $ makeRule g (d : ds)

selector :: Parser SelectorT
selector = do
  x <- simple_selector_sequence
  xs <- many
    (do
        c <- combinator
        s <- simple_selector_sequence
        return $ (L.tokensData c) ++ s)
  return $ x : xs

universal :: Parser [L.Token]
universal =
        try $ do
          s <- count 1 (L._STATIC "*")
          notFollowedBy $ char '|'
          return s
        <|> do
              n <- option [] namespace_prefix
              s <- count 1 (L._STATIC "*")
              return $ n ++ s

_class :: Parser [L.Token]
_class = (:) <$> L._STATIC "." <*> count 1 L._IDENT

element_name :: Parser [L.Token]
element_name = count 1 L._IDENT

attrib :: Parser [L.Token]
attrib = concat <$> sequence [
           count 1 (L._STATIC "["),
           many L._S,
           option [] namespace_prefix,
           many L._S,
           option [] $ concat <$> sequence [
             count 1 L._PREFIXMATCH
             <|> count 1 L._SUFFIXMATCH
             <|> count 1 L._SUBSTRINGMATCH
             <|> count 1 L._INCLUDES
             <|> count 1 L._DASHMATCH
             <|> count 1 (L._STATIC "="),
             many L._S,
             count 1 L._IDENT
             <|> count 1 L._STRING,
             many L._S],
           count 1 (L._STATIC "]")]

pseudo :: Parser [L.Token]
pseudo =
  concat <$> sequence [
    count 1 (L._STATIC ":"),
    option [] (count 1 (L._STATIC ":")),
    try functional_pseudo
    <|> count 1 L._IDENT]


declaration :: Parser (String, String)
declaration =
  do
    x <- do
      p <- property
      count 1 (L._STATIC ":")
      many L._S
      e <- expr
      return $ (tokensData p, tokensData e)
    pr <- option [] prio
    return $ (fst x, (snd x) ++ (tokensData pr))
  <|> return ("", "")

prio :: Parser [L.Token]
prio =  (:) <$> L._IMPORTANT_SYM <*> many L._S



functional_pseudo :: Parser [L.Token]
functional_pseudo = concat <$> sequence [
        count 1 L._FUNCTION,
        many L._S,
        expression,
        count 1 (L._STATIC ")")]

expression :: Parser [L.Token]
expression =
  concat <$> many1 (
    concat <$> sequence [
      count 1 L._PLUS
      <|> count 1 (L._STATIC "-")
      <|> count 1 L._DIMENSION
      <|> count 1 L._NUMBER
      <|> count 1 L._IDENT
      <|> count 1 L._STRING,
      many L._S])

negation :: Parser [L.Token]
negation = concat <$> sequence [
             count 1 L._NOT,
             many L._S,
             negation_arg,
             many L._S,
             count 1 (L._STATIC ")")]

negation_arg :: Parser [L.Token]
negation_arg = try type_selector
                 <|> universal
                 <|> count 1 L._HASH
                 <|> _class
                 <|> pseudo
                 <|> attrib

expr :: Parser [L.Token]
expr = do
        t <- term
        o <- many $
               concat <$> sequence [operator, term]
        return $ t ++ concat o

term :: Parser [L.Token]
term = concat <$> sequence [
        option [] unary_operator,
        try hexcolor
        <|> try ((:) <$> L._UNICODERANGE <*> many L._S)
        <|> try ((:) <$> L._URI <*> many L._S)
        <|> try function
            <|> try ((:) <$> L._PERCENTAGE <*> many L._S)
            <|> try ((:) <$> L._LENGTH <*> many L._S)
            <|> try ((:) <$> L._EMS <*> many L._S)
            <|> try ((:) <$> L._EXS <*> many L._S)
            <|> try ((:) <$> L._ANGLE <*> many L._S)
            <|> try ((:) <$> L._TIME <*> many L._S)
            <|> try ((:) <$> L._FREQ <*> many L._S)
            <|> try ((:) <$> L._NUMBER <*> many L._S)
        <|> try ((:) <$> L._IDENT <*> many L._S)
        <|> (:) <$> L._STRING <*> many L._S]

function :: Parser [L.Token]
function = concat <$> sequence [
  count 1 L._FUNCTION,
  many L._S,
  expr,
  count 1 (L._STATIC ")"),
  many L._S]

hexcolor :: Parser [L.Token]
hexcolor = concat <$> sequence [count 1 L._HASH, many L._S]
