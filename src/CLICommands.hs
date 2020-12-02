module CLICommands where

import Prelude hiding (writeFile)
import System.Exit (exitSuccess)
import Data.Function ((&))
import Data.Functor ((<&>), ($>))
import Data.Foldable (fold)
import Data.Text.IO (writeFile)
import Control.Exception (SomeException, try)
import Data.Typeable (typeOf)
import Data.Maybe (fromMaybe)

import Types (CLICommand(..), CLIState(stocks))
import Prettify (prettifyCmd, prettifyStocks)
import TestStocks (testStocks)
import StocksCompactJSON (toStocksCompactJSON)
import SafeIO (writeFileSafely, readFileSafely, prompt)

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
        effect = (\s -> putStrLn "Done" >> pure s { stocks = testStocks }) },
    CLICommand { 
        name = ["data", "file", "save"],
        description = "Saves the currently-loaded stocks data set to a file", 
        effect = (\s ->
            if (s & stocks & length) == 0
            then putStrLn "No data" >> pure s
            else prompt "Path: "
                >>= (flip writeFileSafely) (s & stocks & toStocksCompactJSON) 
                <&> (\errStr -> case errStr of
                    Just str -> "File write failed\n" ++ str
                    Nothing -> "Done")
                >>= putStrLn
                >> pure s ) },
    CLICommand { 
        name = ["data", "file", "load"],
        description = "Loads a stocks data set from a file", 
        effect = pure }
                
                
                
                
        ]