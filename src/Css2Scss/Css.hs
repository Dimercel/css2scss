{-# LANGUAGE TemplateHaskell #-}

module Css2Scss.Css
    ( Rule(..)
    , selector
    , props
    , Media(..)
    , mediaSel
    , rules
    , Definition(..)
    , defName
    , defValue
    , DefinitionT(..)
    , CssItem (..)
    , Ruleset
    , Property
    , PropertySet
    , SelectorT
    , CompSelector
    , makeRule
    , levelsCount
    , isCompositeRule
    , isChildRule
    , isDirectChildRule
    , isParentRule
    , isFamilyRules
    , toSimpleRule
    ) where


import Data.List (isPrefixOf)
import Data.HashMap ( Map(..)
                    , fromList)
import Data.List.Utils (replace)
import Data.String.Utils (strip)
import Data.Label

import Css2Scss.Utils (eol)


type PropertySet = Map String String
type Property = (String, String)

-- Селектор состоит из групп, каждая из которых
-- в свою очередь, состоит из уровней.
-- Например: .item1 .item2, .item3 .item4 будет
-- представлен так: [[".item1", ".item2"], [".item3", ".item4"]]
type CompSelector = [[String]]
type SelectorT = [String]
type Ruleset = [Rule]

data Rule = Rule { _selector :: CompSelector
                 , _props :: PropertySet
                 } deriving (Show, Eq)

data Media = Media { _mediaSel :: String
                   , _rules :: Ruleset
                   } deriving (Eq, Show)

data DefinitionT = Import | Page | FontFace | Charset | Namespace
                   deriving (Show, Eq)

data Definition = Definition { _defName :: DefinitionT
                             , _defValue :: String
                             } deriving (Eq, Show)

data CssItem = RuleItem Rule | MediaItem Media | DefItem Definition
               deriving (Show, Eq)

mkLabels [''Rule, ''Media, ''Definition]


-- Основной конструктор css-правил. Селектор правила
-- всегда должен находиться в нормальной форме.
makeRule :: CompSelector -> [(String, String)] -> Rule
makeRule sel = Rule (selectorNF sel) . fromList

-- Приводит селектор в нормальную форму.
-- Правила:
-- - никакая часть селектора не содержит переводов строк
-- - уровни селектора не содержат начальных или конечных пробелов
selectorNF :: CompSelector -> CompSelector
selectorNF = map (map (strip . replace eol ""))


-- Для одиночного правила можно посчитать количество
-- уровней в селекторе. Каждый новый пробел означает
-- новый уровень.
-- Пример: ".item1 .item2 p" - 3-х уровневый.
levelsCount :: Rule -> Int
levelsCount rule
  | isCompositeRule rule = 0
  | otherwise = length $ head $ get selector rule

-- Является ли селектор составным?
-- Если в селекторе присутствует запятая,
-- то он считается составным
isCompositeSelector :: CompSelector -> Bool
isCompositeSelector sel = length sel > 1

isCompositeRule :: Rule -> Bool
isCompositeRule = isCompositeSelector . get selector

-- Конвертирует составное правило в список не составных.
-- Все свойства при этом дублируются.
toSimpleRule :: Rule -> Ruleset
toSimpleRule rule = let properties = get props rule
  in map (\x -> Rule [x] properties ) (get selector rule)

-- Является ли первый селектор дочерним, относительно второго?
-- Например: селектор ".item1 .item2 .item3" является дочерним
-- относительно ".item1". Имеются в виду не только прямые потомки.
isChildSelector :: SelectorT -> SelectorT -> Bool
isChildSelector child parent = parent `isPrefixOf` child && parent /= child

isChildRule :: Rule -> Rule -> Bool
isChildRule child parent =
  isChildSelector (head $ get selector child) (head $ get selector parent)

-- Является ли первый селектор прямым потомком второго?
-- Например: ".item1 .item2" является прямым потомком ".item1",
-- а ".item1 .item2_2 .item3" не является относительно ".item1 .item2"
isDirectChildSelector :: SelectorT -> SelectorT -> Bool
isDirectChildSelector child parent =
  length child == length parent + 1 && parent == init child

isDirectChildRule :: Rule -> Rule -> Bool
isDirectChildRule child parent =
  isDirectChildSelector (head $ get selector child) (head $ get selector parent)

-- Является ли первый селектор родительским, относительно второго?
isParentSelector :: SelectorT -> SelectorT -> Bool
isParentSelector parent child = isChildSelector child parent


isParentRule :: Rule -> Rule -> Bool
isParentRule parent child =
  isParentSelector (head $ get selector parent) (head $ get selector child)

-- Принадлежат ли селекторы одной и той же семье?
-- Если селекторы "растут" из одного корня, то они родственники.
-- Например: ".item1", ".item1 .item2", ".item1 .item3" - являются
-- родственными.
isFamilySelectors :: SelectorT -> SelectorT -> Bool
isFamilySelectors x y = head x == head y

isFamilyRules :: Rule -> Rule -> Bool
isFamilyRules x y =
  isFamilySelectors (head $ get selector x) (head $ get selector y)
