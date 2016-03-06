module Css2Scss.Scss
    ( Property(..)
    , Rule(..)
    , Ruleset(..)
    , Variable(..)
    , Mixin(..)
    , Extend(..)
    , isEmptyRule
    , isNotEmptyRule
    ) where

import Data.Tree


data Property = Property { propName :: String, propVal :: String } deriving(Show, Eq)
data Rule     = Rule { selector :: String, ruleProps :: [Property]} deriving(Show, Eq)
type Ruleset  = Tree Rule
data Variable = Variable String String deriving (Show, Eq)
data Mixin    = Mixin String [String] [Property] deriving(Show, Eq)
type Extend   = Rule

isEmptyRule :: Rule -> Bool
isEmptyRule (Rule _ []) = True
isEmptyRule _  = False

isNotEmptyRule :: Rule -> Bool
isNotEmptyRule x = not $ isEmptyRule x
