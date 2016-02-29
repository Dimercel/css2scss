module Css2Scss.Css
    ( Property(..)
    , Ruleset(..)
    , Media(..)
    , Definition(..)
    , buildDefinitions
    , buildRulesets
    , buildMedias
    , chainRuleset
    ) where

import Data.List
import Data.List.Split

import Css2Scss.Css.Lexer as L
import Css2Scss.Css.Parser


data Property   = Property String String deriving(Eq, Show)
data Ruleset    = Ruleset String [Property] deriving(Eq, Show)
data Media      = Media String [Ruleset] deriving (Eq, Show)
data Definition = Definition String String


instance Ord Ruleset where
        compare (Ruleset x _) (Ruleset y _) = compare x y

buildDefinition :: (String, [L.Token]) -> Definition
buildDefinition (id, tokens) = Definition id (L.getTokensData $ L.chomp tokens)

buildDefinitions :: [(String, [L.Token])] -> [Definition]
buildDefinitions [] = []
buildDefinitions x = map (buildDefinition) (filter (isDef) x)
    where isDef d = fst d `elem` ["charset", "import", "namespace", "page", "font-face"]

clearTokensData :: [L.Token] -> String
clearTokensData tokens = L.getTokensData $ L.chomp tokens

getTokBefore :: L.Token -> [L.Token] -> [L.Token]
getTokBefore sep tokens = fst $ span (/= sep) tokens

getTokAfter :: L.Token -> [L.Token] -> [L.Token]
getTokAfter sep tokens = tail $ snd $ span (/= sep) tokens

buildProperty :: [L.Token] -> Property
buildProperty t = Property (identifier t) (value t)
    where identifier x = clearTokensData $ getTokBefore (L.Static, ":") x
          value x = identifier (reverse x)

buildRuleset :: [L.Token] -> Ruleset
buildRuleset x = Ruleset selector properties
    where selector = filter (/= '\n') $ clearTokensData $ getTokBefore (L.Static, "{") x
          properties = map (buildProperty) (endBy [(L.Static, ";")] (getPropTokens x))
          getPropTokens t = L.chomp $ init $ getTokAfter (L.Static, "{") t

buildRulesets :: [(String, [L.Token])] -> [Ruleset]
buildRulesets [] = []
buildRulesets x = map (buildRuleset . snd) (filter (isRuleset) x)
    where isRuleset r = fst r == "ruleset"

chainRuleset :: [L.Token] -> [[L.Token]]
chainRuleset x
        | (L.Static, "}") `notElem` x = []
        | otherwise = let (ruleset, other) = span (/= (L.Static, "}")) x
                          in (concat [ruleset, [(L.Static, "}")]]) : chainRuleset (tail other)

buildMedia :: [L.Token] -> Media
buildMedia x = Media mediaHead rulesets
    where mediaHead = clearTokensData $ getTokBefore (L.Static, "{") x
          rulesets = map (buildRuleset) (chainRuleset body)
          body = L.chomp $ init $ getTokAfter (L.Static, "{") x

buildMedias :: [(String, [L.Token])] -> [Media]
buildMedias [] = []
buildMedias x = map (buildMedia . snd) (filter (isMedia) x)
    where isMedia m = fst m == "media"
