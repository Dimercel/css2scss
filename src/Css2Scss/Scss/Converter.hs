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

module Css2Scss.Scss.Converter (convertCss) where


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
                    , mainSelector
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
--   Этот раздел можно условно поделить на следующие этапы:
-- - Отыскание составных CSS-правил и создание из них одиночных;
-- - Группировка CSS-правил на основании принадлежности к одной "семье";
-- - Собственно построение SCSS-структур;
-- - Поиск с целью объединения одинаковых SCSS-правил;

--   В CSS можно определить правило сразу для нескольких селекторов,
-- тогда они пишутся через запятую. Нам же, для простоты, лучше поделить
-- их на одиночные. Это упростит дальнейший алгоритм.
onlySingleRules :: Ruleset -> Ruleset
onlySingleRules = foldr ((++) . toSimpleRule) []

-- Пример:
-- length $ onlySingleRules $ getSample 1 (S.rulesetWithCompSelector 2 2) => 4

--   Далее нам предстоит сформировать "семьи" правил основываясь
-- на их селекторах. CSS-правила принадлежат одной "семье", если
-- у них есть одинаковый корень в селекторе.
-- Например селекторы: .item1 и .item1 .item2 принадлежат одной семье.
groupByFamily :: Ruleset -> [Ruleset]
groupByFamily = groupBy isFamilyRules

-- Пример:
-- length $ groupByFamily $ getSample 1 (family 3) ++ getSample 42 (family 4) => 2

--   Определим сортировку CSS-правил по уровню селектора.
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

-- Пример:
-- hasOnlyOneRoot $ getSample 1 (family 3) => True

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

--   Теперь мы уже можем построить SCSS-структуру по группе однокоренных CSS-правил
-- образующих семью. Рассмотрим подробнее этот процесс. Прежде всего нам следует
-- отсортировать список правил по уровню селектора. Таким образом "корневое" правило
-- будет в самом начале списка, а его самый дальний дочерний элемент в последнем.
-- На основании этого несложно построить вложенную SCSS-структуру. Нам нужно двигаться
-- от корня к дочерним элементам, попутно создавая вложенные SCSS-узлы. После этого
-- остается лишь привести SCSS-селекторы в нормальную форму.
--   Исходный список должен содержать только однокоренные CSS-правила
-- Например: ".item1", ".item1 .item2" ".item1 .item2 .item3"
convertCssFamily :: Ruleset -> Scss.Rule
convertCssFamily [x] = Node x []
convertCssFamily rules =
  let sortedRules = sortByLevel rules
      go root [] = Node root []
      go root rules =
        let onlyChildren   = filter (`isChildRule` root) rules
            hasParents x   = any (isChildRule x)
            directChildren = filter (\x -> not $ hasParents x onlyChildren) onlyChildren
        in Node root (map (\x -> go x ((\\) rules directChildren)) directChildren)
  in scssNormalize [] $ go (head sortedRules) (tail sortedRules)


--   Когда в начале алгоритма мы делили составные правила на простые, это
-- было необходимо для простоты последующих операций. Теперь же, из-за этого
-- может возникнуть ситуация когда набор свойств SCSS-правила полностью
-- совпадает с другим. Такие правила можно объединить в один SCSS-узел.
--   Стоит упомянуть что такой процесс происходит только для одиночных
-- SCSS-узлов, у которых нет дочерних.
unionBySelector :: Scss.Ruleset -> Scss.Ruleset
unionBySelector [] = []
unionBySelector rules =
  let (compound, single)              = partition Scss.hasChilds rules
      eqProps (Node x _) (Node y _)   = get props x == get props y
      grouped                         = groupBy eqProps single
      unionRules rules@(Node x _ :xs) =
        Node (Rule (map (\(Node r _) -> mainSelector r) rules) (get props x)) []
  in map unionRules grouped ++ compound

-- После описания всех составляющих алгоритма, осталось лишь объединить все
-- шаги и получить результат.
convertCss :: Ruleset -> Scss.Ruleset
convertCss rules =
  let preProcess = foldr (\x acc -> if hasOnlyOneRoot x
                                    then x : acc else acc ++ [[y] | y <- x])
                         [] (groupByFamily $ onlySingleRules rules)
  in unionBySelector $ map convertCssFamily preProcess



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
