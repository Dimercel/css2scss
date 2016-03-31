module Css2Scss.Scss.Render (Renderer(..)) where


import Data.List
import Css2Scss.Scss

eol :: String
eol = "\n"

indentSize :: Int
indentSize = 4

indent :: Int -> String
indent size = replicate indentSize ' '

class Renderer a where
        render :: a -> String
        renderWithIndent :: a -> Int -> String

instance Renderer Property where
        render (Property name val) = name ++ " : " ++ val ++ ";"
        renderWithIndent prop level = indent level ++ (render prop)


