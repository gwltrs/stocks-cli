module PrettifySpec where

import Test.Hspec

import Types (CLICommand(..))
import Prettify (prettifyCmd)

prettifyTests :: SpecWith ()
prettifyTests =
    describe "Prettify.prettifyCmd" $ do
        it "looks pretty" $ do
            prettifyCmd helpMeCmd `shouldBe` "\"help me\": Lists all the commands"
            prettifyCmd downloadCmd `shouldBe` "\"download\": Gets data from the server"

helpMeCmd :: CLICommand
helpMeCmd = CLICommand { name = ["help", "me"], description = "Lists all the commands", effect = pure }
    
downloadCmd :: CLICommand
downloadCmd = CLICommand { name = ["download"], description = "Gets data from the server", effect = pure }