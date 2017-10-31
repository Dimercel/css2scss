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

import qualified Css2Scss.Css as C


-- Scss-правило представляет собой, то же что
-- и css-правило, но с поддержкой древовидной
-- структуры
type Rule     = Tree C.Rule

type Ruleset  = [Tree Rule]

data Variable = Variable { _varIdent
                         , _varValue :: String
                         } deriving(Show, Eq)

type Extend   = Rule

mkLabels [''Variable]
