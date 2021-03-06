module Css2Scss.Css.Sample
  ( getSample
  , selectorLevel
  , selector
  , compositeSelector
  , property
  , propertySet
  , rule
  , ruleset

  , rulesetWithCompSelector
  , family
  ) where


import Data.HashMap ( Map(..)
                    , fromList)
import Data.List (cycle, inits, tail)
import Control.Monad (mapM)
import Test.QuickCheck.Gen
import Test.QuickCheck.Random

import Css2Scss.Css ( Rule(..)
                    , Ruleset
                    , Property
                    , PropertySet
                    , SelectorT
                    , CompSelector)


propertyNameSamples :: [String]
propertyNameSamples =
  [ "margin", "padding", "border", "color"
  , "font-size", "position", "height", "width"
  , "overflow", "font", "cursor"]

propertyValueSamples :: [String]
propertyValueSamples =
  ["absolute", "auto", "black", "white"] ++
  [show x  ++ "px" | x <- [0..100]]

selectorSamples :: [String]
selectorSamples =
  [ ".class-1", ".class-2", ".class-3"
  , "p", "span", "div", "a"]


getSample :: Int -> Gen a -> a
getSample seed (MkGen g) = g (mkQCGen seed) 30

selectorLevel :: Gen String
selectorLevel = elements selectorSamples

selector :: Int -> Gen SelectorT
selector levels =
  do
    l <- infiniteListOf selectorLevel
    return $ take levels l

compositeSelector :: Int -> Gen SelectorT -> Gen CompSelector
compositeSelector = vectorOf


property :: Gen Property
property =
  do
    e <- elements propertyNameSamples
    v <- elements propertyValueSamples
    return (e, v)

propertySet :: Int -> Gen PropertySet
propertySet maxPropCount =
  do
    l <- infiniteListOf property
    return $ fromList (take maxPropCount l)

rule :: Gen CompSelector -> Gen PropertySet -> Gen Rule
rule gSel gProps =
  do
    s <- gSel
    p <- gProps
    return $ Rule s p

ruleset :: Int -> Gen Rule -> Gen Ruleset
ruleset = vectorOf


rulesetWithCompSelector :: Int -> Int -> Gen Ruleset
rulesetWithCompSelector levels count =
  ruleset count $ rule (compositeSelector levels (selector 2)) (propertySet 2)

familySelectors :: Int -> Gen [SelectorT]
familySelectors count =
  do
    l <- shuffle selectorSamples
    let base = take count (cycle l)
    return $ tail $ inits base

family :: Int -> Gen Ruleset
family size =
  do
    selectors <- familySelectors size
    mapM (\sel -> rule (pure [sel]) (propertySet 2)) selectors
