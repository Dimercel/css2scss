module Css2Scss.Scss () where

import Data.Tree

import Css2Scss.Scss.Render
import Css2Scss.Scss.Converter


data Property = Property { name :: String, value :: String } deriving(Show, Eq)
data Rule     = Rule { selector :: String, props :: [Property]} deriving(Show, Eq)
type Ruleset  = Tree Rule
