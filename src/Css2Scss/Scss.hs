{-# LANGUAGE TemplateHaskell #-}

module Css2Scss.Scss
    ( Property(..)
    , Rule(..)
    , Ruleset
    , Variable(..)
    , Mixin(..)
    , Extend
    , name
    , value
    , selector
    , props
    , var_ident
    , var_value
    , mix_name
    , mix_args
    , mix_props
    , isEmptyRule
    , isNotEmptyRule
    ) where

import Data.Tree
import Data.Label
import Test.QuickCheck (Arbitrary(..), arbitrary)


data Property = Property { _name
                         , _value :: String
                         } deriving(Show, Eq)

data Rule     = Rule { _selector :: String
                     , _props :: [Property]
                     } deriving(Show, Eq)

type Ruleset  = Tree Rule

data Variable = Variable { _var_ident
                         , _var_value :: String
                         } deriving(Show, Eq)

data Mixin    = Mixin { _mix_name  :: String
                      , _mix_args  :: [String]
                      , _mix_props :: [Property]
                      } deriving(Show, Eq)

type Extend   = Rule

mkLabels [''Property, ''Rule, ''Variable, ''Mixin]

instance Arbitrary Property where
        arbitrary = do
            name <- arbitrary
            val <- arbitrary
            return $ Property name val

instance Arbitrary Rule where
        arbitrary = do
            selector <- arbitrary
            props <- arbitrary
            return $ Rule selector props

isEmptyRule :: Rule -> Bool
isEmptyRule (Rule _ []) = True
isEmptyRule _  = False

isNotEmptyRule :: Rule -> Bool
isNotEmptyRule x = not $ isEmptyRule x
