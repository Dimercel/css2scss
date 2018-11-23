{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Css2Scss.Scss.Render
  ( MinifyRenderer(..)
  , PrettyRenderer(..)
  ) where

import Data.List (intercalate)
import Data.HashMap (assocs)
import Data.Tree (Tree(..))
import Data.Label

import qualified Css2Scss.Css as Css
import Css2Scss.Scss ( Rule(..)
                     , Definition(..)
                     , DefinitionT(..))
import Css2Scss.Utils (eol)


class PrettyRenderer a where
    renderPretty :: a -> String

instance PrettyRenderer Css.SelectorT where
  renderPretty = unwords

instance PrettyRenderer Css.CompSelector where
  renderPretty = intercalate ("," ++ eol) . map renderPretty

instance PrettyRenderer Css.Property where
  renderPretty (name, val) = name ++ ": " ++ val ++ ";"

instance PrettyRenderer Css.PropertySet where
  renderPretty set =
    let indentSpace = "    "
    in concatMap (\x -> indentSpace ++ renderPretty x ++ eol) (assocs set)

instance PrettyRenderer Css.Rule where
  renderPretty (Css.Rule selector props) =
    concat [
      renderPretty selector,
      " {",
      eol,
      renderPretty props,
      "}",
      eol,
      eol
    ]

instance PrettyRenderer Rule where
  renderPretty rule =
    let renderWithIndent (Node rule subrules) spacer level =
          concat [
            eol,
            levelSpacer,
            renderPretty $ get Css.selector rule,
            " {",
            eol,
            indentProps,
            if null subrules then levelSpacer else eol,
            concatMap (\x -> renderWithIndent x spacer (level + 1)) subrules,
            if null subrules then "" else levelSpacer,
            "}",
            eol
          ]
          where levelSpacer = concat $ replicate level spacer
                renderedProps = map (\x -> renderPretty x ++ eol) (assocs (get Css.props rule))
                indentProps = concatMap (\x -> levelSpacer ++ spacer ++ x) renderedProps
    in renderWithIndent rule "    " 0

instance PrettyRenderer Definition where
  renderPretty (Definition FontFace str) =
    concat [
      eol,
      "@font-face ",
      str,
      eol
    ]
  renderPretty (Definition Import str) =
    concat [
      eol,
      "@import ",
      str,
      eol
    ]
  renderPretty (Definition Page str) =
    concat [
      eol,
      "@page ",
      str,
      eol
    ]
  renderPretty (Definition Charset str) =
    concat [
      eol,
      "@charset ",
      str,
      eol
    ]
  renderPretty (Definition Namespace str) =
    concat [
      eol,
      "@namespace ",
      str,
      eol
    ]

class MinifyRenderer a where
    renderMinify :: a -> String

instance MinifyRenderer Css.SelectorT where
  renderMinify = unwords

instance MinifyRenderer Css.CompSelector where
  renderMinify = intercalate "," . map renderMinify

instance MinifyRenderer Css.Property where
  renderMinify (name, val) = name ++ ":" ++ val ++ ";"

instance MinifyRenderer Css.PropertySet where
  renderMinify set = concatMap renderMinify (assocs set)

instance MinifyRenderer Css.Rule where
  renderMinify (Css.Rule selector props) =
    concat [
      renderMinify selector,
      "{",
      renderMinify props,
      "}"
    ]
