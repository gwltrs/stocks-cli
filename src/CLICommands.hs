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
import Network.Wreq (get, responseBody)
import Control.Lens.Getter ((^.))
import System.Environment (lookupEnv)

import Predundant
import Types
import Prettify (prettifyCmd, prettifyStocks)
import StocksCompactJSON (toStocksCompactJSON, parseStocksCompactJSON)
import SafeIO (writeFileSafely, readFileSafely, prompt)
import Constants (eodhdAPIKeyEnvVar)
import ValidatedLiterals (ValidatedLiterals(..), validatedLiterals)
import EODHD (symbolsURL)

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
            apiKeyRes <- lookupEnv eodhdAPIKeyEnvVar
            case apiKeyRes of
                Nothing -> do
                    putStrLn ("Couldn't find environment variable: " ++ eodhdAPIKeyEnvVar)
                    pure s
                Just apiKey -> do
                    date <- prompt "Get symbols from which date (YYYYMMDD): "
                    case ymd date of
                        Nothing -> do 
                            putStrLn "Invalid date"
                        Just validDate -> do
                            response <- get (symbolsURL apiKey validDate)
                            putStrLn (show response)
                    pure s) },
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
        effect = (\s ->
            prompt "Path: " >>= readFileSafely
                <<&>> parseStocksCompactJSON
                <&> either 
                    (\readErr -> ("File read failed\n" ++ readErr, s))
                    (maybe 
                        ("File isn't in the expected format", s)
                        (\stocks -> ("Done", s { stocks = stocks })))
                >>= (\tup -> putStrLn (fst tup) >> pure (snd tup)) ) } ]