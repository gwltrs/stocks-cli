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
import Control.Lens.Getter ((^.))
import Data.List (intercalate)

import Predundant
import Types
import Railway
import qualified CustomIO as CIO
import Prettify (prettifyCmd, prettifyStocks)
import StocksCompactJSON (toStocksCompactJSON, parseStocksCompactJSON)
import Constants (eodhdAPIKeyEnvVar)
import ValidatedLiterals (ValidatedLiterals(..), validatedLiterals)
import EODHD (symbolsURL, parseSymbols)

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
        effect = (\s -> putStrLn "Done" >> pure s { stocks = validatedLiterals & testStocks }) },
    CLICommand { 
        name = ["data", "fetch", "eodhd"],
        description = "Fetches a stocks data set from",
        effect = (\s -> do
            apiKey <- CIO.lookupEnv eodhdAPIKeyEnvVar
            date <- CIO.prompt "Get symbols from which date (YYYYMMDD): "
                <&> ymd
                ?^| "Invalid date"
            symbols <- (symbolsURL <$> apiKey <*> date)
                >>=| CIO.getBody
                >>>?| (parseSymbols, "Couldn't parse symbols")
            putStrLn $ either id show $ symbols
            pure s )},
    CLICommand {
        name = ["data", "file", "save"],
        description = "Saves the currently-loaded stocks data set to a file",
        effect = (\s ->
            ifTrue ((s & stocks & length) == 0) "No data"
                >>^| CIO.prompt "Path: "
                >>>=| (flip CIO.writeFile) (s & stocks & toStocksCompactJSON)
                >>>. (id, const "Done")
                >>= putStrLn
                >> pure s ) },
    CLICommand { 
        name = ["data", "file", "load"],
        description = "Loads a stocks data set from a file", 
        effect = (\s ->
            (CIO.prompt "Path: " >>= CIO.readFile)
                >>>?| (parseStocksCompactJSON, "Wrong format")
                >>>=. (
                    \err -> putStrLn err >> pure s,
                    \stocks -> putStrLn "Done" >> pure s { stocks = stocks } ) ) } ]