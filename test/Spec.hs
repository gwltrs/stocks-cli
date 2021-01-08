import Test.Hspec

import Types
import CLISpec (cliTests)
import PrettifySpec (prettifyTests)
import StocksCompactCSVSpec (stocksCompactCSVTests)
import TypesSpec (typesTests)
import ValidatedLiteralsSpec (validatedLiteralsTests)
import EODHDSpec (eodhdTests)
import PredundantSpec (predundantTests)
import IndyParsingSpec (indyParsingTests)
import IndyComposingSpec (indyComposingTests)
import IndyCommandsSpec (indyCommandsTests)

main :: IO ()
main = hspec $ do
    cliTests
    prettifyTests
    stocksCompactCSVTests
    typesTests
    validatedLiteralsTests
    eodhdTests
    predundantTests
    indyParsingTests
    indyComposingTests
    indyCommandsTests