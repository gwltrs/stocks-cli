import Test.Hspec

import Types
import CLISpec (cliTests)
import PrettifySpec (prettifyTests)
import StocksCompactJSONSpec (stocksCompactJSONTests)
import TypesSpec (typesTests)
import ValidatedLiteralsSpec (validatedLiteralsTests)
import EODHDSpec (eodhdTests)

main :: IO ()
main = hspec $ do
    cliTests
    prettifyTests
    stocksCompactJSONTests
    typesTests
    validatedLiteralsTests
    eodhdTests