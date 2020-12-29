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
import Control.Category ((>>>))
import Control.Error.Util ((??))
import Data.Char (isAlpha)
import qualified Data.Vector as V
import qualified Data.Vector.NonEmpty as NEV

import Predundant
import Types
import qualified CustomIO as CIO
import Prettify (prettifyCmd, prettifyStocks)
import StocksCompactCSV (toStocksCompactCSV, parseStocksCompactCSV)
import Constants (eodhdAPIKeyEnvVar)
import ValidatedLiterals (ValidatedLiterals(..), validatedLiterals)
import EODHD (symbolsURL, parseSymbols, daysURL, parseDays)

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
        effect = (\s -> CIO.printAndReturn s $ do
            apiKey <- CIO.lookupEnv eodhdAPIKeyEnvVar
            date <- CIO.promptSymbolsDate
            year <- CIO.promptDaysStartingYear
            putStrLn "Fetching symbols..." & CIO.liftM
            symbols <- CIO.getBody (symbolsURL apiKey date)
                >>= (\body -> parseSymbols body ?? "Couldn't parse symbols")
                -- Only keeping symbols that are entirely alphabetic and are less 
                -- than 5 characters in length. This eliminates symbols that 
                -- can't be traded on mainstread platforms such as Robinhood.
                <&> V.filter (not . any (not . isAlpha))
                <&> V.filter ((<= 4) . length)
            putStrLn ("Got " ++ (show $ length $ symbols) ++ " symbols")
                & CIO.liftM
            stocks <- CIO.getStocks apiKey year symbols
                & CIO.liftM
            putStrLn ("Got " ++ (show $ length $ stocks) ++ " stocks")
                >> pure s { stocks = stocks }
                & CIO.liftM )},
    CLICommand {
        name = ["data", "file", "save"],
        description = "Saves the currently-loaded stocks data set to a file",
        effect = (\s -> CIO.printAndReturn s $
            if s & stocks & null
            then CIO.returnError "No data"
            else do
                _ <- CIO.prompt "Path: "
                    & CIO.liftM
                    >>= (flip CIO.writeFile) (s & stocks & toStocksCompactCSV)
                CIO.liftM (putStrLn "Done" >> pure s) ) },
    CLICommand { 
        name = ["data", "file", "load"],
        description = "Loads a stocks data set from a file", 
        effect = (\s -> CIO.printAndReturn s $ do
            fileText <- CIO.prompt "Path: "
                & CIO.liftM 
                >>= CIO.readFile
            stocks <- parseStocksCompactCSV fileText ?? "Wrong format"
            putStrLn "Done"
                >> pure s { stocks = stocks }
                & CIO.liftM ) } ]