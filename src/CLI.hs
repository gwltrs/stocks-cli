module CLI where

import Types
import Control.Monad (forever)
import System.IO
import Data.List.Extra (trim, lower)
import Data.Function ((&))
import Data.Functor ((<&>), ($>))
import Maybes (firstJusts, orElse)
import System.Exit (exitSuccess)
import Data.Foldable (fold)

runCLI :: IO ()
runCLI = do
    putStrLn ("Technical Analysis CLI")
    putStrLn ("\"help\" for list of commands")
    getAndRunLinesForever (CLIState { stocks = Nothing })

getAndRunLinesForever :: CLIState -> IO ()    
getAndRunLinesForever state = do
    (putStr ">>> " >> hFlush stdout)
    getLine >>= runLine state >>= getAndRunLinesForever

runLine :: CLIState -> String -> IO CLIState
runLine state cmdStr =
    let 
        parsedCmd = commands & firstCommandMatch cmdStr <&> effect <&> ($state)
        printError = putStrLn "Unrecognized command\n\"help\" for list of commands" >> pure state
    in
        parsedCmd `orElse` printError

commands :: [CLICommand]
commands = [
    CLICommand { 
        name = ["help"],
        description = "Prints the list of available commands", 
        effect = commands <&> helpText <&> putStrLn & fold & ($>) },
    CLICommand { 
        name = ["quit"],
        description = "Terminates the application", 
        effect = (exitSuccess $>) }, 
    CLICommand { 
        name = ["fetch", "eodhd"],
        description = "Fetches daily charts data from eodhistoricaldata.com", 
        effect = (exitSuccess $>) } ]

-- 
helpText :: CLICommand -> String
helpText = undefined

-- Finds the first command that matches the user's input
firstCommandMatch :: String -> [CLICommand] -> Maybe CLICommand
firstCommandMatch = undefined