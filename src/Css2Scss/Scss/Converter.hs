{-# LANGUAGE OverloadedStrings #-}

-- |
-- Данный модуль непосредственно занимается конвертацией CSS структур в SCSS.
-- При конвертации используются следующие алгоритмы: автоматическое создание
-- переменных, построение вложенных структур стилей, поиск расширений.
--
-- Автоматическое создание переменных: работает только для значений цветов,
-- алгоритм сканирует исходную CSS структуру и анализирует использование 
-- одинаковых цветовых значений в свойствах стиля. Если кол-во использований
-- одного и того же цветового значения больше некоторого порога, то создается
-- переменная с именем @color-n (n - номер по счету) в которой содержится
-- это значение.
--
-- Построение вложенных структур: вложенные SCSS структуры строятся на
-- основе анализа CSS селекторов. Результирующая структура представляет
-- собой типичное дерево, содержащие внутри себя иерархию стилей.
--
-- Поиск расширений: ищет наборы одинаковых свойств и их значений внутри
-- стилей. Такие наборы определяются в одно правило и затем используются по
-- средствам SCSS директивы @extend.

module Css2Scss.Scss.Converter
    ( makeVariables
    , buildSCSSRulesets
    ) where

import Text.Regex.PCRE.Light (match, compile)
import qualified Data.ByteString.Char8 as S
import qualified Data.Map as Map
import Data.Maybe
import Data.List
import Data.Tree

import Css2Scss.Css
import qualified Css2Scss.Scss as SC

type GroupingStyles = [Ruleset]

findColorInProp :: Property -> Maybe String
findColorInProp (Property _ val) = case match (compile "#\\d{3,6}" []) (S.pack val) [] of
                       Just m -> Just (S.unpack $ head $ m)
                       Nothing -> Nothing

-- | Пороговое значение при котором создаются переменные. Если цветовое значение
-- встречалось меньшее кол-во раз, то для нее переменная не создается.
colorLimit :: Int
colorLimit = 3

makeVariables :: [Ruleset] -> [SC.Variable]
makeVariables rulesets = buildVariables $ colorStat
                         $ map (fromJust) (concat $ map (findColors) rulesets)
    where findColors (Ruleset _ props) = filter (/= Nothing) (map (findColorInProp) props)
          colorStat colors = filter (\x -> snd x >= colorLimit)
              (map (\x -> (head x, length x)) (group $ sort colors))
          buildVariables stat = map (\x -> variable (snd x) (fst x))
              (zipWith (\x y -> (fst x, y)) stat [0 ..])
          variable n val = SC.Variable ("color-" ++ show n) val

toSCSSProp :: Property -> SC.Property
toSCSSProp (Property name val) = SC.Property name val

toSCSSRule :: Ruleset -> SC.Rule
toSCSSRule (Ruleset selector props) = SC.Rule selector (map (toSCSSProp) props)

-- | Проверяет, явялется ли первый селектор подселектором второго.
-- Например: body li > a является подселектором body li. Особо следует
-- отметить тот случай, что селектор не является своим собственным
-- подселектором.
isSubSelector :: Ruleset -> Ruleset -> Bool
isSubSelector (Ruleset x _) (Ruleset y _)
        | x == y = False
        | otherwise = y == take (length y) x

-- | Селекторы стилей должны принадлежать одной группе, если для них
-- существует один общий подселектор. Сам этот подселектор, также должен
-- входить в группу.
isOneGroup :: Ruleset -> Ruleset -> Bool
isOneGroup first@(Ruleset x _) second@(Ruleset y _)
        | length x > length y = isSubSelector first second
        | otherwise = isSubSelector second first

-- | Группирует селекторы стилей на основе предиката isOneGroup
groupSelectors :: [Ruleset] -> [[Ruleset]]
groupSelectors rulesets = groupBy (isOneGroup) (sort rulesets)


-- | На основе одной группы css-стилей строит древовидную структуру
-- SCSS-стилей.
buildSCSSRuleset :: GroupingStyles -> SC.Ruleset
buildSCSSRuleset rules
        | length rules == 1 = Node (toSCSSRule prefix) []
        | otherwise = Node (toSCSSRule prefix)
            (map (buildSCSSRuleset) (groupSelectors xs))
        where (prefix : xs) = rules

-- | Группирует стили и строит на их основе древовидную структуру SCSS стилей
buildSCSSRulesets :: [Ruleset] -> [SC.Ruleset]
buildSCSSRulesets ruleset = map (buildSCSSRuleset) (groupSelectors ruleset)
