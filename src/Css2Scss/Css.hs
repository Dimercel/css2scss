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

isSpaceToken :: L.Token -> Bool
isSpaceToken x = fst x == L.S

-- | Обрезает токены пробельных символов в начале
-- и конце списка
chomp :: [L.Token] -> [L.Token]
chomp x = dropWhileEnd (isSpaceToken) (dropWhile (isSpaceToken) x)

buildDefinition :: (String, [L.Token]) -> Definition
buildDefinition (id, tokens) = Definition id (L.getTokensData tokens)

buildDefinitions :: [(String, [L.Token])] -> [Definition]
buildDefinitions [] = []
buildDefinitions x = map (buildDefinition) (filter (isDef) x)
    where isDef d = fst d `elem` ["charset", "import", "namespace", "page", "font-face"]

buildProperty :: [L.Token] -> Property
buildProperty x = Property identifier value
    where getIdent t = L.getTokensData $ chomp $ fst t
          identifier = getIdent $ span (/= (L.Static, ":")) x
          value = getIdent $ span (/= (L.Static, ":")) (reverse x)

buildRuleset :: [L.Token] -> Ruleset
buildRuleset x = Ruleset selector properties
    where getIdent t = L.getTokensData $ chomp $ fst t
          selector = getIdent $ span (/= (L.Static, "{")) x
          getPropTokens t = chomp $ tail $ init $ snd $ span (/= (L.Static, "{")) t
          properties = map (buildProperty) (endBy [(L.Static, ";")] (getPropTokens x))

buildRulesets :: [(String, [L.Token])] -> [Ruleset]
buildRulesets [] = []
buildRulesets x = map (buildRuleset . snd) (filter (isRuleset) x)
    where isRuleset r = fst r == "ruleset"

mediaHead :: [L.Token] -> String
mediaHead x = L.getTokensData $ chomp $ fst $ span (/= (L.Static, "{")) x

chainRuleset :: [L.Token] -> [[L.Token]]
chainRuleset x
        | (L.Static, "}") `notElem` x = []
        | otherwise = let (ruleset, other) = span (/= (L.Static, "}")) x
                          in (concat [ruleset, [(L.Static, "}")]]) : chainRuleset (tail other)

buildMedia :: [L.Token] -> Media
buildMedia x = Media (mediaHead x) rulesets
    where body = chomp $ tail $ init $ snd $ span (/= (L.Static, "{")) x
          rulesets = map (buildRuleset) (chainRuleset body)

buildMedias :: [(String, [L.Token])] -> [Media]
buildMedias [] = []
buildMedias x = map (buildMedia . snd) (filter (isMedia) x)
    where isMedia m = fst m == "media"
