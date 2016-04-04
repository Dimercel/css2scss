{-# LANGUAGE FlexibleInstances #-}

module Css2Scss.Scss.Render
    ( Renderer(..)
    , PrettyRenderer(..)
    ) where


import Data.List
import Data.Tree
import Css2Scss.Scss


eol :: String
eol = "\n"

indentSize :: Int
indentSize = 4

indent :: Int -> String
indent level = replicate (indentSize * level) ' '

class Renderer a where
        render :: a -> String

class PrettyRenderer a where
        renderPretty :: Int -> a -> String

instance Renderer Property where
        render (Property name val) = name ++ " : " ++ val ++ ";"

instance Renderer Rule where
        render (Rule sel props) = sel ++ " {" ++ propsText ++ "}"
            where propsText = intercalate " " (map (render) props)

instance Renderer (Tree Rule) where
        render (Node rule []) = render rule
        render (Node (Rule sel props) rules) =
            concat [sel, " {", propsText, (concat $ map (render) rules), " }"]
            where propsText = intercalate (" ") (map (render) props)


instance PrettyRenderer Property where
        renderPretty level prop = indent level ++ (render prop)

instance PrettyRenderer Rule where
        renderPretty level (Rule sel props) =
            concat [indent level, sel, " {", eol, propsText, eol, indent level, "}", eol]
            where propsText = intercalate (eol) (map (renderPretty (level+1)) props)

instance PrettyRenderer (Tree Rule) where
        renderPretty level (Node rule []) = renderPretty level rule
        renderPretty level (Node (Rule sel props) rules) =
            concat [eol, indent level, sel, " {",
                    eol, propsText, eol, eol,
                    (concat $ map (renderPretty (level + 1)) rules),
                    indent level, "}", eol]
            where propsText = intercalate (eol) (map (renderPretty (level + 1)) props)
