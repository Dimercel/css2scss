import Test.Hspec
import Css2Scss.Test.Lexer as L
import Css2Scss.Test.Parser as P

main :: IO ()
main = do
    L.run
    P.run
