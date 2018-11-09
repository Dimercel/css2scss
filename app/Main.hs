module Main where

import System.Environment
import System.IO
import Text.ParserCombinators.Parsec
import Css2Scss.Css.Parser ( preprocessor
                           , stylesheet)
import Css2Scss.Css as C
import Css2Scss.Scss.Render (PrettyRenderer(..))
import Css2Scss.Scss.Converter (convertCss)


main :: IO ()
main = do
  args <- getArgs
  handle <- openFile (head args) ReadMode
  contents <- hGetContents handle
  let tokens = parse stylesheet "" (preprocessor contents)
  case tokens of
      Right t -> putStrLn $ concatMap renderPretty (convertCss $ onlyRules t)
      Left err -> print err
  hClose handle
  where
    isRule (C.RuleItem _) = True
    isRule _ = False
    onlyRules x = map (\(C.RuleItem x) -> x) (filter isRule x)
