module CLISpec where

import Test.Hspec
import Control.Exception (evaluate)
import Data.Function ((&))
import Data.Functor ((<&>))

import CLI (firstCommandMatch)
import Types

cliTests :: SpecWith ()
cliTests =
    let
        shouldParseToDescription :: String -> Maybe String -> Expectation
        shouldParseToDescription userInput descriptionMaybe =
            (firstCommandMatch testCommands userInput <&> description) `shouldBe` descriptionMaybe
    in
        describe "CLI.firstCommandMatch" $ do
            it "returns Nothing" $ do
                "asdf"                     `shouldParseToDescription` Nothing
                ""                         `shouldParseToDescription` Nothing
                "YE ET"                    `shouldParseToDescription` Nothing
                "launchmissles"            `shouldParseToDescription` Nothing
            it "matches yeet command" $ do
                "   yeet      \n\t     "   `shouldParseToDescription` (Just "1") 
                "Yeet\n"                   `shouldParseToDescription` (Just "1")
            it "matches first launch missles command" $ do
                " LaUnCh\n\tMiSsLeS"       `shouldParseToDescription` (Just "2")
                " launch\tMISSLES\n\n\n\n" `shouldParseToDescription` (Just "2")
                -- Making sure it doesn't skip to the second "launch missles"
                "launch missles"           `shouldParseToDescription` (Just "2")

testCommands :: [CLICommand]
testCommands = [
    CLICommand { name = ["yeet"], description = "1", effect = pure },
    CLICommand { name = ["launch", "missles"], description = "2", effect = pure },
    CLICommand { name = ["launch", "missles"], description = "3", effect = pure } ]