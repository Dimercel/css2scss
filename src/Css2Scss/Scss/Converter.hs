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

module Css2Scss.Scss.Converter (toScssRules) where


import Data.List ( find
                 , groupBy
                 , partition
                 , sortBy
                 , zipWith
                 , (\\))
import Data.Char (toUpper)
import Data.Tree (Tree(..))
import Data.Label
import Data.HashMap ( Map(..)
                    , elems
                    , empty
                    , findWithDefault
                    , fromList
                    , insertWith
                    , unionWith
                    , union
                    , updateWithKey)
import qualified Data.HashMap (map)
import Text.ParserCombinators.Parsec

import Css2Scss.Css.Parser ( stylesheet
                           , hexcolor)
import Css2Scss.Css ( Rule(..)
                    , makeRule
                    , selector
                    , props
                    , Ruleset
                    , SelectorT
                    , toSimpleRule
                    , isCompositeRule
                    , isChildRule
                    , isFamilyRules)
import qualified Css2Scss.Scss as Scss


-- Устраняет составные селекторы, деля их на одиночные
onlySingleRules :: Ruleset -> Ruleset
onlySingleRules = foldr ((++) . toSimpleRule) []

-- Вернет список сгруппированных по семейной принадлежности правил.
-- Правила образуют семейство, если у них общий корень.
-- Например: .item1 и .item1 .item2 принадлежат одной семье.
groupByFamily :: Ruleset -> [Ruleset]
groupByFamily = groupBy isFamilyRules

-- Сортирует список css-правил по уровню селектора.
-- Каждый пробел в селекторе составляет новый уровень.
sortByLevel :: [Rule] -> [Rule]
sortByLevel =
  let eqByLevel x y = compare (length $ head $ get selector x)
                              (length $ head $ get selector y)
  in sortBy eqByLevel

-- Есть ли в указанном списке правило, являющееся родителем
-- относительно всех остальных
hasOnlyOneRoot :: [Rule] -> Bool
hasOnlyOneRoot rules =
  let withOutElem x = filter (x /=)
  in any (\x -> all (`isChildRule` x) (withOutElem x rules)) rules

-- Данная функция убирает не нужные части из имен селекторов.
-- Scss - древовидная структура, так что корень имени селектора
-- не должен дублироваться в дочерних селекторах. Функция
-- устраняет такое дублирование.
scssNormalizeSel :: SelectorT -> Scss.Rule -> Scss.Rule
scssNormalizeSel root (Node (Rule sel props) subRules) =
  Node (Rule [(\\) (head sel) root] props)
       (map (scssNormalizeSel (head sel)) subRules)

-- Строит иерархическую scss-структуру на основе css-правил.
-- Список должен содержать только однокоренные css-правила
-- Например: ".item1", ".item1 .item2" ".item1 .item2 .item3"
cssFamily2Scss :: [Rule] -> Scss.Rule
cssFamily2Scss [x] = Node x []
cssFamily2Scss rules =
  let sortedRules = sortByLevel rules
      toScss root [] = Node root []
      toScss root rules =
        let onlyChildren = filter (`isChildRule` root) rules
            hasParents x = any (isChildRule x)
            directChildren = filter (\x -> not $ hasParents x onlyChildren) onlyChildren
        in Node root (map (\x -> toScss x ((\\) rules directChildren)) directChildren)
  in scssNormalizeSel [] $ toScss (head sortedRules) (tail sortedRules)

-- Если набор свойств scss-правила полностью совпадает
-- с другим, то такие правила можно представить одним с
-- составным селектором. Функция отыскивает такие правила
-- и объединяет их в одно.
groupBySelector :: Scss.Ruleset -> Scss.Ruleset
groupBySelector [] = []
groupBySelector rules =
  let (compound, single) = partition Scss.hasChilds rules
      getProps (Node rule _) = get props rule
      getSelector (Node rule _) = get selector rule
      grouped = groupBy (\x y -> getProps x == getProps y) single
      unionRules r = Node (Rule (map (head . getSelector) r) (getProps $ head r)) []
  in map unionRules grouped ++ compound

-- Конвертирует список css-правил в список scss-структур
toScssRules :: [Rule] -> Scss.Ruleset
toScssRules rules =
  let preProcess = foldr (\x acc -> if hasOnlyOneRoot x
                                    then x : acc else acc ++ [[y] | y <- x])
                   [] (groupByFamily $ onlySingleRules rules)
  in groupBySelector $ map cssFamily2Scss preProcess

-- Является ли строка hex-представлением цвета?
isColorValue :: String -> Bool
isColorValue str =
  case parse hexcolor "" str of
    Right _ -> True
    Left  _ -> False

-- Цвет может храниться в форме #fff или #ffffff.
-- Данная функция преобразует #fff -> #ffffff
toFullHexColorForm :: String -> String
toFullHexColorForm str
  | length str == 4 = map toUpper (str ++ tail str)
  | otherwise = str

-- В значениях свойств могут встречаться цветовые характеристики.
-- Данная функция находит такие значения и возвращает хэш со
-- статистикой о них. Ключами являются hex-представления цветов, а
-- значениями количество найденных элементов такого цвета.
findColorValues :: Scss.Rule -> Map String Int
findColorValues (Node rule subrules) =
  let properties = get props rule
      incCount hash key = insertWith (\_ y -> y + 1) key 1 hash
      -- Статистика вхождений цветов. Ключ - hex-значение,
      -- значение - кол-во совпадений. Пример: "#fff" => 1
      stat = foldl (\acc x -> if isColorValue x
                      then incCount acc (toFullHexColorForm x)
                      else acc)
             empty (elems properties)
  in foldl (\acc x -> unionWith (+) x acc)
           stat (map findColorValues subrules)

-- В Scss есть понятие переменной. После поиска цветовых значений
-- нужно построить scss-переменные представляющие их. Данная
-- функция строит эти переменные по переданному списку уникальных
-- цветовых значений.
makeVariablesByColor :: [String] -> [Scss.Variable]
makeVariablesByColor colors =
  zipWith (\x y -> Scss.Variable ("color" ++ show y) x) colors [0..]

-- Подменяет цветовые значения на имена scss-переменных, переданных в
-- списке.
replaceColorOnVariable :: Scss.Rule -> [Scss.Variable] -> Scss.Rule
replaceColorOnVariable r vars =
  let colors = fromList $ map (\(Scss.Variable id val) -> (val, "$" ++ id)) vars
      replaceProcess (Node rule subrules) =
        let properties = get props rule
            newProps   = Data.HashMap.map (\x -> findWithDefault x x colors) properties
        in Node (Rule (get selector rule) newProps) (map replaceProcess subrules)
  in replaceProcess r
