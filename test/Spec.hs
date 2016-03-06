import Test.Hspec
import Css2Scss.Test.Lexer as L
import Css2Scss.Test.Parser as P
import Css2Scss.Test.Scss as SC

main :: IO ()
main = do
    L.run
    P.run
    SC.run
