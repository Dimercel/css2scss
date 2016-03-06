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
    , buildExtends
    ) where

import Text.Regex.PCRE.Light (match, compile)
import qualified Data.ByteString.Char8 as S
import qualified Data.Map as Map
import Data.Maybe
import Data.List
import Data.Ord
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
          variable n val = SC.Variable ("color" ++ show n) val

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
        | length rules == 1 = Node (toSCSSRule subselector) []
        | otherwise = Node (toSCSSRule subselector)
            (map (buildSCSSRuleset) (groupSelectors xs))
        where (subselector : xs) = rules

-- | Группирует стили и строит на их основе древовидную структуру SCSS стилей
buildSCSSRulesets :: [Ruleset] -> [SC.Ruleset]
buildSCSSRulesets ruleset = map (buildSCSSRuleset) (groupSelectors ruleset)

getProps :: Ruleset -> [Property]
getProps (Ruleset _ props) = props

numberingRules :: [SC.Extend] -> [SC.Extend]
numberingRules x = map (addNum) (zip x [0 ..])
        where addNum ((SC.Rule name p), n) = SC.Rule (name ++ (show n)) p 

-- | Находиит и собирает в единый список все расшинения по переданному
-- набору CSS правил
buildRawExtends :: [Ruleset] -> [SC.Extend]
buildRawExtends [] = []
buildRawExtends rulesets = convert (findExtend rulesets) : buildRawExtends (tail rulesets)
    where convert props = SC.Rule "@extend" (map (toSCSSProp) props)
          findExtend (x:[]) = []
          findExtend (x:xs) = limit $ maximumBy (comparing length) $
              map (\y -> maxSubSequence (getProps x) (getProps y) ) xs
          limit e = if length e > 2 then e else []

buildExtends :: [Ruleset] -> [SC.Extend]
buildExtends rulesets = postProcess $ buildRawExtends rulesets
    where postProcess x = numberingRules $ uniq $ notEmpty $ x
          uniq = nub
          notEmpty = filter (SC.isNotEmptyRule)

-- | Возвращает максимальную, общую подпоследовательность в двух списках
maxSubSequence :: (Eq a) => [a] -> [a] -> [a]
maxSubSequence xs ys = reverse . maximumBy (comparing length) .
    concat $ [f xs' ys | xs' <- tails xs] ++ [f xs ys' | ys' <- tail $ tails ys]
        where f xs ys = scanl g [] $ zip xs ys
              g z (x, y) = if x == y then x:z else []
