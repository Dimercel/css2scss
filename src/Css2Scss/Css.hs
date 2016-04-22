{-# LANGUAGE TemplateHaskell #-}

module Css2Scss.Css
    ( Property(..)
    , Ruleset(..)
    , Media(..)
    , Definition(..)
    , buildDefinitions
    , buildRulesets
    , buildMedias
    , chainRuleset
    , getTokBefore
    , getTokAfter
    , name
    , value
    , selector
    , props
    , mediaSel
    , rules
    , defName
    , defValue
    ) where

import Data.List
import Data.List.Split
import Control.Category ((.), id)
import Data.Label
import Prelude hiding ((.), id)


import Css2Scss.Css.Lexer ( Token
                          , TokenId(Static)
                          , getTokensData
                          , chomp)


data Property = Property { _name
                         , _value :: String
                         } deriving (Eq, Show)

data Ruleset = Ruleset { _selector :: String
                       , _props :: [Property]
                       } deriving (Eq, Show)

data Media = Media { _mediaSel :: String
                   , _rules :: [Ruleset]
                   } deriving (Eq, Show)

data Definition = Definition { _defName
                             , _defValue :: String
                             } deriving (Eq, Show)

mkLabels [''Property, ''Ruleset, ''Media, ''Definition]

instance Ord Ruleset where
        compare (Ruleset x _) (Ruleset y _) = compare x y

buildDefinition :: (String, [Token]) -> Definition
buildDefinition (id, tokens) = Definition id (getTokensData $ chomp tokens)

buildDefinitions :: [(String, [Token])] -> [Definition]
buildDefinitions [] = []
buildDefinitions x = map (buildDefinition) (filter (isDef) x)
    where isDef d = fst d `elem` ["charset", "import", "namespace", "page", "font-face"]

clearTokensData :: [Token] -> String
clearTokensData tokens = getTokensData $ chomp tokens

getTokBefore :: Token -> [Token] -> [Token]
getTokBefore sep tokens = fst $ span (/= sep) tokens

getTokAfter :: Token -> [Token] -> [Token]
getTokAfter sep tokens = case snd $ span (/= sep) tokens of
                                  [] -> []
                                  x -> tail x

buildProperty :: [Token] -> Property
buildProperty t = Property (identifier t) (value t)
    where identifier x = clearTokensData $ getTokBefore (Static, ":") x
          value x = identifier (reverse x)

buildRuleset :: [Token] -> Ruleset
buildRuleset x = Ruleset selector properties
    where selector = filter (/= '\n') $ clearTokensData $ getTokBefore (Static, "{") x
          properties = map (buildProperty) (endBy [(Static, ";")] (getPropTokens x))
          getPropTokens t = chomp $ init $ getTokAfter (Static, "{") t

buildRulesets :: [(String, [Token])] -> [Ruleset]
buildRulesets [] = []
buildRulesets x = map (buildRuleset . snd) (filter (isRuleset) x)
    where isRuleset r = fst r == "ruleset"

chainRuleset :: [Token] -> [[Token]]
chainRuleset x
        | (Static, "}") `notElem` x = []
        | otherwise = let (ruleset, other) = span (/= (Static, "}")) x
                          in (concat [ruleset, [(Static, "}")]]) : chainRuleset (tail other)

buildMedia :: [Token] -> Media
buildMedia x = Media mediaHead rulesets
    where mediaHead = clearTokensData $ getTokBefore (Static, "{") x
          rulesets = map (buildRuleset) (chainRuleset body)
          body = chomp $ init $ getTokAfter (Static, "{") x

buildMedias :: [(String, [Token])] -> [Media]
buildMedias [] = []
buildMedias x = map (buildMedia . snd) (filter (isMedia) x)
    where isMedia m = fst m == "media"
