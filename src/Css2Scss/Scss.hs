module Css2Scss.Scss
    ( Property(..)
    , Rule(..)
    , Ruleset(..)
    , Variable(..)
    , Mixin(..)
    ) where

import Data.Tree


data Property = Property { propName :: String, propVal :: String } deriving(Show, Eq)
data Rule     = Rule { selector :: String, ruleProps :: [Property]} deriving(Show, Eq)
type Ruleset  = Tree Rule
data Variable = Variable String String deriving (Show, Eq)
data Mixin    = Mixin String [String] [Property] deriving(Show, Eq)
type Extend   = Rule
