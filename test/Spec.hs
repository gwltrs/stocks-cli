import Test.Hspec

import Types
import CLISpec (cliTests)

main :: IO ()
main = hspec $ do
    cliTests