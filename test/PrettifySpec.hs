module PrettifySpec where

import Test.Hspec

import Types (CLICommand(..), Stock(..))
import Prettify (prettifyCmd, prettifyStocks, prettifyDate)
import TestStocks (testStocks)

prettifyTests :: SpecWith ()
prettifyTests = do
    describe "Prettify.prettifyCmd" $ do
        it "looks pretty" $ do
            prettifyCmd helpMeCmd `shouldBe` "\"help me\": Lists all the commands"
            prettifyCmd downloadCmd `shouldBe` "\"download\": Gets data from the server"
    describe "Prettify.prettifyStocks" $ do
        it "shows 0 stocks" $ do
            prettifyStocks [] `shouldBe` "0 stocks"
            prettifyStocks stocksWithoutDays `shouldBe` "0 stocks"
        it "shows 2 stocks" $ do
            prettifyStocks testStocks `shouldBe` "2 stocks from 1/2/2019 to 10/1/2020"
    describe "Prettify.prettifyDate" $ do
        it "looks pretty" $ do
            prettifyDate "00010203" `shouldBe` "2/3/1"
            prettifyDate "20190723" `shouldBe` "7/23/2019"
            prettifyDate "20201209" `shouldBe` "12/9/2020"

helpMeCmd :: CLICommand
helpMeCmd = CLICommand { name = ["help", "me"], description = "Lists all the commands", effect = pure }
    
downloadCmd :: CLICommand
downloadCmd = CLICommand { name = ["download"], description = "Gets data from the server", effect = pure }

stocksWithoutDays :: [Stock]
stocksWithoutDays = [
    Stock { symbol = "A", days = [] },
    Stock { symbol = "B", days = [] },
    Stock { symbol = "C", days = [] } ]