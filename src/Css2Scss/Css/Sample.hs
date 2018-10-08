module Css2Scss.Scss.Converter () where


import Data.HashMap ( Map(..)
                    , fromList)
import Test.QuickCheck.Gen
import Test.QuickCheck.Random

import Css2Scss.Css ( Rule(..)
                    , Ruleset
                    , Property
                    , PropertySet)


getSample :: Int -> Gen a -> a
getSample seed (MkGen g) = g (mkQCGen seed) 30

selectorLevel :: Gen String
selectorLevel = elements [ ".class-1", ".class-2", ".class-3"
                         , "p", "span", "div", "a"]

selector :: Int -> Gen [String]
selector levels =
  do
    l <- infiniteListOf selectorLevel
    return $ take levels l

property :: Gen Property
property =
  do
    e <- elements [ "margin", "padding", "border", "color"
                  , "font-size", "position", "height", "width"
                  , "overflow", "font", "cursor"]
    v <- elements (["absolute", "auto", "black", "white"] ++ [show x  ++ "px" | x <- [0..100]])
    return (e, v)

propertySet :: Int -> Gen PropertySet
propertySet maxPropCount =
  do
    l <- infiniteListOf property
    return $ fromList (take maxPropCount l)
