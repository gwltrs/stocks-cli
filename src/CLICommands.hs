module CLICommands where

import System.Exit (exitSuccess)
import Data.Function ((&))
import Data.Functor ((<&>), ($>))
import Data.Foldable (fold)

import Types (CLICommand(..), CLIState(stocks))
import Prettify (prettifyCmd, prettifyStocks)
import TestStocks (testStocks)

helpName :: [String]
helpName = ["help"]

cliCommands :: [CLICommand]
cliCommands = [
    CLICommand { 
        name = helpName,
        description = "Prints the list of available commands", 
        effect = cliCommands <&> prettifyCmd <&> putStrLn & fold & ($>) },
    CLICommand { 
        name = ["quit"],
        description = "Terminates the application", 
        effect = (exitSuccess $>) },
    CLICommand { 
        name = ["data", "show"],
        description = "Prints the currently-loaded stocks data set", 
        effect = (\s -> s & stocks & prettifyStocks & putStrLn >> pure s) },
    CLICommand { 
        name = ["data", "fetch", "sample"],
        description = "Fetches the small, built-in stocks data set", 
        effect = (\s -> putStrLn "Done" >> pure s { stocks = testStocks }) } ]