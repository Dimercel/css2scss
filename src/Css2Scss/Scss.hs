{-# LANGUAGE TemplateHaskell #-}

module Css2Scss.Scss
    ( Rule(..)
    , Ruleset
    , Variable(..)
    , Extend
    , varIdent
    , varValue
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

type Extend   = Rule

mkLabels [''Variable]
