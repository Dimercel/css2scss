-- |
-- Данный модуль непосредственно занимается конвертацией CSS структур в SCSS.
-- При конвертации используются следующие алгоритмы: автоматическое создание
-- переменных, построение вложенных структур стилей, поиск расширений.
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
import Data.Char (toLower)
import Data.Tree (Tree(..))
import Data.Label
import Data.HashMap ( Map(..)
                    , elems
                    , empty
                    , findWithDefault
                    , fromList
                    , insertWith
                    , unionWith)
import qualified Data.HashMap (map)
import Text.ParserCombinators.Parsec

import Css2Scss.Css.Parser ( stylesheet
                           , hexcolor)
import Css2Scss.Css ( Rule(..)
                    , Ruleset
                    , SelectorT
                    , levelsCount
                    , isChildRule
                    , isFamilyRules
                    , props
                    , selector
                    , toSimpleRule)
import qualified Css2Scss.Scss as Scss


-- | ПОСТРОЕНИЕ SCSS-СТРУКТУР.
--
--
--   В этом разделе нам предстоит построить SCSS-структуры на основе CSS-стилей.
-- Основными вопросами здесь будут построение древовидной структуры SCSS на
-- основе CSS-правил, а также вопрос принадлежности их к одной "семье".
--   Этот раздел можно условно поделить на 4 этапа:
-- - Отыскание составных CSS-правил и создание из них одиночных;
-- - Группировка CSS-правил на основании принадлежности к одной "семье";
-- - Собственно построение SCSS-структур;
-- - Поиск с целью объединения одинаковых SCSS-правил;

--   В CSS можно определить правило сразу для нескольких селекторов,
-- тогда они пишутся через запятую. Нам же, для простоты, лучше поделить
-- их на одиночные. Это упростит дальнейший алгоритм.
onlySingleRules :: Ruleset -> Ruleset
onlySingleRules = foldr ((++) . toSimpleRule) []

--   Далее нем предстоит сформировать "семьи" правил основываясь
-- на их селекторах. CSS-правила принадлежат одной "семье", если
-- у них есть одинаковый корень в селекторе.
-- Например селекторы: .item1 и .item1 .item2 принадлежат одной семье.
groupByFamily :: Ruleset -> [Ruleset]
groupByFamily = groupBy isFamilyRules

-- Сортирует список css-правил по уровню селектора.
-- Каждый пробел в селекторе составляет новый уровень.
sortByLevel :: Ruleset -> Ruleset
sortByLevel =
  let eqByLevel x y = compare (levelsCount x)
                              (levelsCount y)
  in sortBy eqByLevel

-- Есть ли в указанном списке CSS-правило, являющееся родителем
-- относительно всех остальных?
hasOnlyOneRoot :: Ruleset -> Bool
hasOnlyOneRoot rules =
  let withoutElem x = filter (x /=)
  in any (\x -> all (`isChildRule` x) (withoutElem x rules)) rules

--   При построении SCSS-структуры стоит заметить, что в исходных CSS-правилах
-- все селекторы абсолютны. То есть селектор всегда описывает путь от самого
-- корня и до оконечного звена. Это является следствием линейной структуры CSS.
--   В SCSS же напротив, при вложенности селекторы не содержат корень, а лишь
-- относительный путь. В качестве опорной точки выступает родительское правило.
-- Следовательно нам придется убрать корень из всех селекторов SCSS-иерархии,
-- кроме очевидно, самого первого звена. Этим определяется нормальная форма SCSS.
scssNormalize :: SelectorT -> Scss.Rule -> Scss.Rule
scssNormalize root (Node (Rule sel props) subRules) =
  Node (Rule [(\\) (head sel) root] props)
       (map (scssNormalize (head sel)) subRules)

-- Строит иерархическую scss-структуру на основе css-правил.
-- Список должен содержать только однокоренные css-правила
-- Например: ".item1", ".item1 .item2" ".item1 .item2 .item3"
cssFamily2Scss :: Ruleset -> Scss.Rule
cssFamily2Scss [x] = Node x []
cssFamily2Scss rules =
  let sortedRules = sortByLevel rules
      toScss root [] = Node root []
      toScss root rules =
        let onlyChildren   = filter (`isChildRule` root) rules
            hasParents x   = any (isChildRule x)
            directChildren = filter (\x -> not $ hasParents x onlyChildren) onlyChildren
        in Node root (map (\x -> toScss x ((\\) rules directChildren)) directChildren)
  in scssNormalize [] $ toScss (head sortedRules) (tail sortedRules)

-- Если набор свойств scss-правила полностью совпадает
-- с другим, то такие правила можно представить одним
-- составным селектором. Функция отыскивает такие правила
-- и объединяет их в одно.
groupBySelector :: Scss.Ruleset -> Scss.Ruleset
groupBySelector [] = []
groupBySelector rules =
  let (compound, single)        = partition Scss.hasChilds rules
      getProps (Node rule _)    = get props rule
      getSelector (Node rule _) = get selector rule
      grouped      = groupBy (\x y -> getProps x == getProps y) single
      unionRules r = Node (Rule (map (head . getSelector) r) (getProps $ head r)) []
  in map unionRules grouped ++ compound

-- Конвертирует список css-правил в список scss-структур
toScssRules :: Ruleset -> Scss.Ruleset
toScssRules rules =
  let preProcess = foldr (\x acc -> if hasOnlyOneRoot x
                                    then x : acc else acc ++ [[y] | y <- x])
                         [] (groupByFamily $ onlySingleRules rules)
  in groupBySelector $ map cssFamily2Scss preProcess



-- | АВТОМАТИЧЕСКОЕ СОЗДАНИЕ ПЕРЕМЕННЫХ.
--
--
--   В этом разделе описывается алгоритм поиска значений цветов CSS-стилей и
-- создание на их основе SCSS-переменных.
--   Рассмотрим более подробно. После процесса парсинга получается список
-- содержащий все CSS-стили из указанного файла. Наш алгоритм работает следующим
-- образом: сканирует все CSS-правила и анализирует использование одинаковых
-- цветовых значений в свойствах стиля. Если количество вхождений одного и
-- того же цветового значения больше некоторого порога, то создается переменная с
-- именем @color-n (n - номер по счету) в которой содержится это значение.


-- Прежде всего нам понадобится отличать цветовые значения от всех остальных.
-- Мы ограничимся только hex-представлениями цвета.
isColorValue :: String -> Bool
isColorValue str =
  case parse hexcolor "" str of
    Right _ -> True
    Left  _ -> False

-- Далее стоит учесть, что значение представляющие цвет хранится в форме #fff или #ffffff.
-- Такая двойственность нам не нужна и мы конвертируем #fff -> #ffffff.
toFullHexColorForm :: String -> String
toFullHexColorForm str
  | length str == 4 = map toLower (str ++ tail str)
  | otherwise       = map toLower str

-- Теперь мы вооружены всем чтобы приступить к подсчету вхождений цветов.
-- Собираем простейшую статистику состоящую из наименования цвета и количества
-- его вхождений в правило. Возвращаем информацию в виде хеша.
findColorValues :: Scss.Rule -> Map String Int
findColorValues (Node rule subrules) =
  let properties        = get props rule
      incCount hash key = insertWith (\_ y -> y + 1) key 1 hash
      -- Статистика вхождений цветов. Ключ - hex-значение,
      -- значение - кол-во совпадений. Пример: "#ffffff" => 1
      stat = foldl (\acc x -> if isColorValue x
                      then incCount acc (toFullHexColorForm x)
                      else acc)
             empty (elems properties)
  in foldl (\acc x -> unionWith (+) x acc)
           stat (map findColorValues subrules)

-- После подсчета статистики вхождений цветов мы уже можем построить
-- SCSS-переменные. Имена этих переменных будут иметь формат @color-n,
-- где n служит уникальным идентификатором переменной.
makeVariablesByColor :: [String] -> [Scss.Variable]
makeVariablesByColor colors =
  zipWith (\x y -> Scss.Variable ("color-" ++ show y) x) colors [0..]

-- Все что осталось это заменить цветовые значения на соответствующие SCSS-переменные.
replaceColorOnVariable :: Scss.Rule -> [Scss.Variable] -> Scss.Rule
replaceColorOnVariable r vars =
  let colors = fromList $ map (\(Scss.Variable id val) -> (val, "$" ++ id)) vars
      replaceProcess (Node rule subrules) =
        let properties = get props rule
            newProps   = Data.HashMap.map (\x -> findWithDefault x x colors) properties
        in Node (Rule (get selector rule) newProps) (map replaceProcess subrules)
  in replaceProcess r
