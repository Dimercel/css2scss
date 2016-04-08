{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

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
    ( buildVariables
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
import Data.Char (isSpace)
import Control.Applicative

import Css2Scss.Css
import qualified Css2Scss.Scss as SC

type GroupingStyles = [Ruleset]

class Convertable a b where
        convert :: a -> b

instance Convertable Property SC.Property where
        convert (Property name val) = SC.Property name val

instance Convertable SC.Property Property where
        convert (SC.Property name val) = Property name val

instance Convertable Ruleset SC.Rule where
        convert (Ruleset selector props) = SC.Rule selector (map convert props)

instance Convertable SC.Rule Ruleset where
        convert (SC.Rule selector props) = Ruleset selector (map convert props)

-- | Обрезает пробельные символы в начале и конце строки
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

-- | Отыскивает цветовое значение в css-свойстве и возвращает его в виде
-- строки
findColorInProp :: Property -> Maybe String
findColorInProp (Property _ val) = case match (compile "#\\d{3,6}" []) (S.pack val) [] of
                       Just m -> Just (S.unpack $ head $ m)
                       Nothing -> Nothing

-- | Ищет одинаковые значения цветов и добавляеи их в результирующую
-- выборку если они проходят ограничение
filterColorsByLimit :: [String] -> Int -> [String]
filterColorsByLimit colors limit = map (fst) (limitFilter colorCount)
              where limitFilter = filter (\x -> snd x >= limit)
                    colorCount = map (\x -> (head x, length x)) (group $ sort colors)

-- | Уникализирует имена scss-переменных, путем добавления числового
-- индекса к их имени
numberingColors :: [String] -> [SC.Variable]
numberingColors colors = map buildVariable (zip colors [0..])
        where buildVariable (color, n) = SC.Variable ("color" ++ show n) color

-- | Ищет одинаковые значения цветов и на основе этой информации формирует
-- список scss-переменных
buildVariables :: [Ruleset] -> [SC.Variable]
buildVariables rulesets = numberingColors $ filterColorsByLimit (foundColors) 3
    where searchColors (Ruleset _ props) = filter (/= Nothing) (map (findColorInProp) props)
          foundColors = map (fromJust) (concat $ map (searchColors) rulesets)

-- | Проверяет, явялется ли первый селектор подселектором второго.
-- Например: body li > a является подселектором body li. Особо следует
-- отметить тот случай, что селектор не является своим собственным
-- подселектором.
isSubSelector :: Ruleset -> Ruleset -> Bool
isSubSelector (Ruleset x _) (Ruleset y _)
        | x == y = False
        | otherwise = (isPrefixOf y x) && (isPrefixOf (y ++ " ") x)

-- | TODO: Требуется учитывать тот случай, когда селектор является составным.
-- Нужно рассматривать каждую его часть отдельно или перегруппировывать.

-- | Селекторы стилей должны принадлежать одной группе, если для них
-- существует один общий подселектор. Сам этот подселектор, также должен
-- входить в группу.
isOneGroup :: Ruleset -> Ruleset -> Bool
isOneGroup first@(Ruleset x _) second@(Ruleset y _)
        | ',' `elem` (concat [x, y]) = False
        | length x > length y = isSubSelector first second
        | otherwise = isSubSelector second first

-- | Группирует селекторы стилей на основе предиката isOneGroup
groupSelectors :: [Ruleset] -> [[Ruleset]]
groupSelectors rulesets = groupBy (isOneGroup) (sort rulesets)


-- | На основе одной группы css-стилей строит древовидную структуру
-- SCSS-стилей.
buildSCSSRuleset :: GroupingStyles -> SC.Ruleset
buildSCSSRuleset rules
        | length rules == 1 = Node (convert subselector) []
        | otherwise = Node (convert subselector)
            (map (buildSCSSRuleset) (groupSelectors xs))
        where (subselector : xs) = rules

-- | Группирует стили и строит на их основе древовидную структуру SCSS стилей
buildSCSSRulesets :: [Ruleset] -> [SC.Ruleset]
buildSCSSRulesets ruleset = map (normalizeSelectors . buildSCSSRuleset)
        (groupSelectors ruleset)

-- | При конвертации из css-стилей в scss-селекторы, начальная часть имен
-- селекторов может дублироваться, т.к. css не поддерживает древовидные
-- структуры. Данная функция устраняет такое дублирование, тем самым строя
-- верную сьруктуру дерева.
normalizeSelectors :: SC.Ruleset -> SC.Ruleset
normalizeSelectors leaf@(Node _ []) = leaf
normalizeSelectors (Node rule childRules) =
        Node rule (map normalizeSelectors normChilds)
        where normChilds = map
                  (\(Node r c) -> (Node (normSelector r) c)) childRules
              normSelector x = SC.Rule
                  (childSelector (SC.selector rule) (SC.selector x))
                  (SC.ruleProps x)

-- | Возвращает корректное имя scss-селектора. Убирает из дочернего селектора
-- имя родительского. Если одинаковых частей в селекторах нет, то
-- возвращается дочернее имя селектора без каких-либо изменений. Функция не
-- работает с группой селекторов (когда имена указаны через ',')
childSelector :: String -> String -> String
childSelector parent child
    | ',' `elem` (concat [parent, child]) = child
    | otherwise = case head <$> prefix of
                      Just ' ' -> trim $ fromJust prefix
                      _   -> child
        where prefix = stripPrefix parent child

getProps :: Ruleset -> [Property]
getProps (Ruleset _ props) = props

-- | Уникализирует имена расширений по средствам добавления к имени 
-- числового индекса
numberingRules :: [SC.Extend] -> [SC.Extend]
numberingRules x = map (addNum) (zip x [0 ..])
        where addNum ((SC.Rule name p), n) = SC.Rule (name ++ (show n)) p 

-- | Находиит и собирает в единый список все расшинения по переданному
-- набору CSS правил
buildRawExtends :: [Ruleset] -> [SC.Extend]
buildRawExtends [] = []
buildRawExtends rulesets = convertRules (findExtend rulesets) : buildRawExtends (tail rulesets)
    where convertRules props = SC.Rule "@extend" (map convert props)
          findExtend (x:[]) = []
          findExtend (x:xs) = limit $ maximumBy (comparing length) $
              map (\y -> maxSubSequence (getProps x) (getProps y) ) xs
          limit e = if length e > 2 then e else []

-- | Дополнительно обрабатывает "сырые" расширения. Гарантирует
-- уникальность, а также сортировку от большего кол-ва css-свойств
-- к меньшему.
buildExtends :: [Ruleset] -> [SC.Extend]
buildExtends rulesets = postProcess $ buildRawExtends rulesets
    where postProcess x = reverse $ sortBy (comparing (length . SC.ruleProps)) $
              numberingRules $ uniq $ notEmpty $ x
          uniq = nub
          notEmpty = filter (SC.isNotEmptyRule)

-- | Содержится ли указанное расширение в правилах
contentExtend :: SC.Extend -> Ruleset -> Bool
contentExtend (SC.Rule _ extProps) (Ruleset _ props) = extProps `isSubsequenceOf` ruleProps
        where ruleProps = map convert props

replaceExtend :: SC.Extend -> Ruleset -> Ruleset
replaceExtend ext@(SC.Rule extName extProps) rule@(Ruleset ruleName props)
    | not $ contentExtend ext rule = rule
    | otherwise = Ruleset ruleName (map convert withExtend)
    where ruleProps = map convert props
          withExtend = (SC.Property "@extend" extName) : filter (`notElem` extProps) ruleProps

replaceAllExtends :: [SC.Extend] -> [Ruleset] -> [Ruleset]
replaceAllExtends [] rules = rules
replaceAllExtends exts rules = replaceAllExtends (tail exts)
    (map (\x -> replaceExtend (head exts) x) rules)

-- | Возвращает максимальную, общую подпоследовательность в двух списках
maxSubSequence :: (Eq a) => [a] -> [a] -> [a]
maxSubSequence xs ys = reverse . maximumBy (comparing length) .
    concat $ [f xs' ys | xs' <- tails xs] ++ [f xs ys' | ys' <- tail $ tails ys]
        where f xs ys = scanl g [] $ zip xs ys
              g z (x, y) = if x == y then x:z else []
