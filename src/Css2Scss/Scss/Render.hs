{-# LANGUAGE FlexibleInstances #-}

module Css2Scss.Scss.Render (Renderer(..)) where


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
        renderWithIndent :: Int -> a -> String

instance Renderer Property where
        render (Property name val) = name ++ " : " ++ val ++ ";"
        renderWithIndent level prop = indent level ++ (render prop)

instance Renderer Rule where
        render (Rule sel props) = sel ++ " {" ++ propsText ++ "}"
            where propsText = intercalate " " (map (render) props)

        renderWithIndent level (Rule sel props) =
            concat [indent level, sel, " {", eol, propsText, eol, indent level, "}", eol]
            where propsText = intercalate (eol) (map (renderWithIndent (level+1)) props)

instance Renderer (Tree Rule) where
        render (Node rule []) = render rule
        render (Node (Rule sel props) rules) =
            concat [sel, " {", propsText, (concat $ map (render) rules), " }"]
            where propsText = intercalate (" ") (map (render) props)

        renderWithIndent level (Node rule []) = renderWithIndent level rule
        renderWithIndent level (Node (Rule sel props) rules) =
            concat [eol, indent level, sel, " {",
                    eol, propsText, eol, eol,
                    (concat $ map (renderWithIndent (level + 1)) rules),
                    indent level, "}", eol]
            where propsText = intercalate (eol) (map (renderWithIndent (level + 1)) props)

instance Renderer Variable where
        render (Variable name val) = concat ["$", name, ": ", val, ";"]

        renderWithIndent level var = concat [indent level, render var]
