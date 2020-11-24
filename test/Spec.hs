import Test.Hspec

import Types
import CLISpec (cliTests)
import PrettifySpec (prettifyTests)

main :: IO ()
main = hspec $ do
    cliTests
    prettifyTests