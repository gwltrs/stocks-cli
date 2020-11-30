import Test.Hspec

import Types
import CLISpec (cliTests)
import PrettifySpec (prettifyTests)
import StocksCompactJSONSpec (stocksCompactJSONTests)

main :: IO ()
main = hspec $ do
    cliTests
    prettifyTests
    stocksCompactJSONTests