{-# LANGUAGE TemplateHaskell #-}

module Css2Scss.Scss
    ( Rule(..)
    , Ruleset
    , Variable(..)
    , Definition(..)
    , DefinitionT(..)
    , Extend
    , varIdent
    , varValue
    , hasChilds
    ) where

import Data.Tree (Tree(..))
import Data.Label
import Test.QuickCheck (Arbitrary(..), arbitrary)

import qualified Css2Scss.Css as Css


-- Scss-правило представляет собой, то же что
-- и css-правило, но с поддержкой древовидной
-- структуры
type Rule     = Tree Css.Rule
type Ruleset  = [Rule]
data Variable = Variable { _varIdent
                         , _varValue :: String
                         } deriving(Show, Eq)

data DefinitionT = Import | Page | FontFace | Charset | Namespace
                   deriving (Show, Eq)

data Definition = Definition { _defName :: DefinitionT
                             , _defValue :: String
                             } deriving (Eq, Show)


type Extend   = Rule

mkLabels [''Variable]


hasChilds :: Rule -> Bool
hasChilds (Node _ childs) = not $ null childs
